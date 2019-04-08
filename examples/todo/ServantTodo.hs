{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module ServantTodo where

import           Conduit
import           Control.Concurrent                      (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.Lifted           as STM
import           Control.Monad                           (forM_, forever,
                                                          unless, void, when)
import           Control.Monad.Catch                     (catch)
import           Crypto.Random                           (drgNew)
import           Data.Aeson                              (Value, decode, toJSON)
import           Data.Aeson.Text
import           Data.ByteString                         (ByteString)
import           Data.Char                               (isDigit)
import           Data.Coerce                             (coerce)
import           Data.Conduit
import           Data.Default                            (def)
import qualified Data.HashMap.Strict                     as HashMap

import           Data.Text                               (Text)
import           Data.Text.Encoding                      (decodeUtf8,
                                                          encodeUtf8)
import           Data.Text.Lazy.Builder                  (toLazyText)
import           Data.Time                               (defaultTimeLocale,
                                                          formatTime)
import           Data.Time.Clock                         (UTCTime (..),
                                                          getCurrentTime)
import           Fay.Convert                             (readFromFay',
                                                          showToFay)
import           Network.Wai                             (Application)
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets                      hiding (Headers)
import           Prelude                                 hiding (interact)
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth        (mkAuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           Servant.Server.StaticFiles
import           System.Directory                        (createDirectoryIfMissing,
                                                          doesFileExist,
                                                          getModificationTime,
                                                          listDirectory,
                                                          removeFile)
import           System.FilePath.Posix                   ((<.>), (</>))
import           System.Random                           (randomRIO)
import           Text.Blaze.Front.Html5                  (toValue, (!))
import           Text.Blaze.Front.Renderer               (renderNewMarkup)
import           Text.Blaze.Html5                        (Html)

import qualified Data.ByteString.Base64                  as Base64
import qualified Data.ByteString.Char8                   as BSC8
import qualified Data.ByteString.Lazy                    as BL
import qualified Data.Conduit.List
import qualified Data.List                               as L
import qualified Data.Text                               as T
import qualified Text.Blaze.Front.Event                  as E
import qualified Text.Blaze.Front.Html5                  as H
import qualified Text.Blaze.Front.Html5.Attributes       as A

import           Bridge
import           Shared
import           Web.Front                               (createTask)

type API = Header "cookie" T.Text :> Get '[HTML] (Cookied Html)
  :<|> "static" :> Raw

-- * App

data Config = Config
  { model              :: TVar Model
  , channel            :: TChan (Out (Action Msg))
  , static             :: FilePath
  , clients            :: TVar [Int]
  -- cookie settings
  , authCookieSettings :: AuthCookieSettings
  , generateKey        :: (IO ())  -- ^ An action to create a new key
  , randomSource       :: RandomSource
  , serverKeySet       :: FileKeySet
  }

-- * Model

data Model = Model
  { entries :: [Entry]
  , nextId  :: Int
  }

data Entry = Entry
  { description :: Text
  , eid         :: Int
  }

-- * API

initModel :: Model
initModel = Model { entries = [], nextId = 0 }

api :: Proxy API
api = Proxy

server :: Config -> Server API
server cfg@Config{..} = serveRoot cfg :<|> serveStatic cfg
  where
    addSession' = addSession
      authCookieSettings -- the settings
      randomSource       -- random source
      serverKeySet       -- server key set

    serveRoot cfg'@Config{..} _mclient = do
      (clientId, state) <- liftIO $ do
        clientId <- clientSession cfg' _mclient
        setSession cfg' clientId
        state <- readTVarIO model
        pure (clientId, state)
      addSession'
        (def { ssExpirationType = MaxAge })
        clientId
        (renderNewMarkup $ do
        H.html $ do
          H.head $ do
            H.title "TODO"
            H.script ! A.src "/static/bundle.js" $ ""
          H.body $ do
            H.div ! A.id "root" $ renderModel state)

    serveStatic Config{..} = serveDirectoryWebApp static

addClient :: IO Int
addClient = randomRIO (0, 1000000)

interact cfg stream in' out' tvar = \client -> do
  race_
    (writeLoop stream out' client)
    (readLoop cfg stream in' tvar client)

  where
    writeLoop _stream _out _client = forever $ liftIO $ do
      cmd <- atomically $ readTChan _out
      json <- pure $ toJSON . showToFay $ cmd
      case cmd of
        EmptyCmd ->
          sendTextData _stream (toLazyText $ encodeToTextBuilder json)
        ExecuteClient cid task strategy -> do
          let sid = _client
          if sid == cid && strategy == ExecuteExcept
            then do
              json2 <- pure $ toJSON $ showToFay $
                ExecuteClient cid task ExecuteExcept
              sendTextData _stream (toLazyText $ encodeToTextBuilder json2)
            else do
              json2 <- pure $ toJSON $ showToFay $
                ExecuteClient cid task ExecuteAll
              sendTextData _stream (toLazyText $ encodeToTextBuilder json2)

    readLoop _cfg _stream _in _tvar _client = forever $ liftIO $ do
      data' <- receiveData _stream
      runConduit $ yield data' .| mapM_C (\cmdstr -> do
        case (decode $ BL.fromChunks [encodeUtf8 cmdstr] :: Maybe Value) of
          Nothing -> error "No JSON provided"
          Just cmd -> do
            liftIO $ putStrLn $ show cmd
            res <- onCommand cmd _tvar _cfg _client
            atomically $ writeTChan _in res)

-- * Event Handling

onCommand cmd stateTVar = \cfg client -> do
  m@Model{..} <- STM.readTVarIO stateTVar
  case readFromFay' cmd of
    Right (Send (Action _ _ acmd)) -> do
      case acmd of
        Add -> do
          let newTodo = Entry "TODO: " nextId
              newState = Model (newTodo : entries) (succ nextId)
              task = createTask "root" renderModel newState
          atomically $ void $ swapTVar stateTVar newState
          return (ExecuteClient client task ExecuteAll)
        Complete _eid -> do
          let newState = Model (filter ((/=_eid) . eid) entries) nextId
              task = createTask "root" renderModel newState
          atomically $ void $ swapTVar stateTVar newState
          return (ExecuteClient client task ExecuteAll)
        Update _eid val -> do
          let newState = Model ((upd _eid val) <$> entries) nextId
              upd _id val' e@Entry{..} =
                if eid == _id then e { description = val', eid = _id } else e
              task = createTask "root" renderModel newState
          atomically $ void $ swapTVar stateTVar newState
          return (ExecuteClient client task ExecuteExcept)
    Right AskEvents -> do
      let task = createTask "root" renderModel m
      return $ ExecuteClient client task ExecuteAll

-- * Rendering

renderModel :: Model -> H.Markup (Action Msg)
renderModel Model{..} = do
  H.h1 $ "TODO MVC: Servant"
  H.br
  forM_ entries $ \entry -> renderEntry entry
  let btnId = "todo-add"
  H.button
    ! A.id "todo-add"
    ! A.type_ "submit"
    ! E.onClick (Action btnId ObjectAction Add)
    $ "Add"

renderEntry :: Entry -> H.Markup (Action Msg)
renderEntry Entry{..} = do
  let elemId = "todo-" <> (T.pack $ show eid)
      removeId = "remove-" <> (T.pack $ show eid)
  H.input
    ! A.id (toValue elemId)
    ! A.type_ "text"
    ! A.value (toValue description)
    ! E.onKeyUp (Action elemId RecordAction (Update eid ""))
  H.button
    ! A.id (toValue removeId)
    ! A.type_ "submit"
    ! E.onClick (Action removeId ObjectAction (Complete eid))
    $ "Done"
  H.br

-- * Helpers

checkSession cfg =
  clientSession cfg . fmap decodeUtf8 . HashMap.lookup "cookie"
  . HashMap.fromList . requestHeaders . pendingRequest

clientSession :: Config -> Maybe Text -> IO Int
clientSession cfg@Config{..} mclient = do
  result <- lookupSession cfg mclient
  case result of
    Nothing  -> addClient >>= (\x -> set cfg x >> pure x)
    Just cid -> pure cid
  where
    set conf clientNum = setSession conf clientNum

lookupSession :: Config -> Maybe Text -> IO (Maybe Int)
lookupSession Config{..} = \client -> do
  case client of
    Nothing  -> pure Nothing
    Just c   -> do
      msession <- getHeaderSession authCookieSettings serverKeySet c `catch` ex
      case epwSession <$> msession of
        Nothing     -> pure Nothing
        Just client -> pure $ pure client
  where
    ex :: AuthCookieExceptionHandler IO
    ex _e = pure Nothing

setSession Config{..} clientId = atomically $ do
  ids <- readTVar clients
  unless (clientId `elem` ids) $ modifyTVar' clients (clientId :)

parseInt :: Text -> Int
parseInt = toInt . ignoreChars

toInt :: String -> Int
toInt a = read a :: Int

ignoreChars :: Text -> String
ignoreChars = L.filter isDigit . T.unpack

safeFromJust :: forall t. t -> Maybe t -> t
safeFromJust defv Nothing = defv
safeFromJust _ (Just a)   = a

-- * Main

main = do
  let fksp = FileKSParams
        { fkspKeySize = 16
        , fkspMaxKeys = 3
        , fkspPath = "./test-key-set"
        }
  cfg <- Config
    <$> STM.newTVarIO initModel
    <*> atomically newBroadcastTChan
    <*> pure "./static"
    <*> newTVarIO []
    -- cookie settings
    <*> (pure $ def { acsCookieFlags = ["HttpOnly"] })
    <*> pure (mkFileKey fksp)
    <*> mkRandomSource drgNew 1000
    <*> (mkFileKeySet fksp)

  putStrLn "Server up and running on http://localhost:3000/"
  run 3000 $ app cfg

-- | Custom handler that bluntly reports any occurred errors.
authHandler :: AuthCookieHandler (Maybe Int)
authHandler acs sks = mkAuthHandler $ \request ->
  (getSession acs sks request) `catch` handleEx >>= maybe
    (throwError err403 {errBody = "No cookies"})
    (return)
  where
    handleEx :: AuthCookieExceptionHandler Handler
    handleEx ex = throwError err403 {errBody = BL.fromStrict . BSC8.pack $ show ex}

app :: Config -> Application
app cfg@Config{..} = websocketsOr defaultConnectionOptions wsApp mainApp
  where
    wsApp :: ServerApp
    wsApp pendingConn = do
      let writeChan' = channel
      _client <- checkSession cfg pendingConn
      stream <- acceptRequest pendingConn
      forkPingThread stream 60 -- Ping
      readChan' <- atomically $ dupTChan writeChan'
      interact cfg stream writeChan' readChan' model (_client)
    mainApp = serveWithContext
      (Proxy :: Proxy API)
      ((authHandler authCookieSettings serverKeySet) :. EmptyContext)
      (server cfg)

----------------------------------------------------------------------------
-- KeySet
-- A custom implementation of a keyset on top of 'RenewableKeySet'.
-- Keys are stored as files with base64 encoded data in 'test-key-set' directory.
-- To add a key just throw a file into the directory.
-- To remove a key delete corresponding file in the directory.
-- Both operations can be performed via web interface (see '/keys' page).


data FileKSParams = FileKSParams
  { fkspPath    :: FilePath
  , fkspMaxKeys :: Int
  , fkspKeySize :: Int
  }

data FileKSState = FileKSState
  { fkssLastModified :: UTCTime } deriving Eq

type FileKeySet = RenewableKeySet FileKSState FileKSParams

mkFileKey :: FileKSParams -> IO ()
mkFileKey FileKSParams{..} = (,) <$> mkName <*> mkKey >>= uncurry writeFile where

  mkKey = generateRandomBytes fkspKeySize
    >>= return
      . BSC8.unpack
      . Base64.encode

  mkName = getCurrentTime
    >>= return
      . (fkspPath </>)
      . (<.> "b64")
      . formatTime defaultTimeLocale "%0Y%m%d%H%M%S"
    >>= \name -> do
      exists <- doesFileExist name
      if exists
        then (threadDelay 1000000) >> mkName
        -- ^ we don't want to change the keys that often
        else return name


mkFileKeySet :: (MonadIO m, MonadThrow m)
  => FileKSParams
  -> m (RenewableKeySet FileKSState FileKSParams)
mkFileKeySet = mkKeySet where

  mkKeySet FileKSParams {..} = do
    liftIO $ do
      createDirectoryIfMissing True fkspPath
      listDirectory fkspPath >>= \fs -> when (null fs) $
        mkFileKey FileKSParams {..}

    let fkssLastModified = UTCTime (toEnum 0) 0

    mkRenewableKeySet
      RenewableKeySetHooks {..}
      FileKSParams {..}
      FileKSState {..}

  rkshNeedUpdate FileKSParams {..} (_, FileKSState {..}) = do
    lastModified <- liftIO $ getModificationTime fkspPath
    return (lastModified > fkssLastModified)

  getLastModifiedFiles FileKSParams {..} = listDirectory fkspPath
    >>= return . map (fkspPath </>)
    >>= \fs -> zip <$> (mapM getModificationTime fs) <*> (return fs)
    >>= return
      . map snd
      . L.take fkspMaxKeys
      . L.reverse
      . L.sort

  readKey = fmap (either (error "wrong key format") id . Base64.decode . BSC8.pack) . readFile

  rkshNewState FileKSParams {..} (_, s) = liftIO $ do
    lastModified <- getModificationTime fkspPath
    keys <- getLastModifiedFiles FileKSParams {..} >>= mapM readKey
    return (keys, s {fkssLastModified = lastModified})

  rkshRemoveKey FileKSParams {..} key = liftIO $ getLastModifiedFiles FileKSParams {..}
    >>= \fs -> zip fs <$> mapM readKey fs
    >>= return . filter ((== key) . snd)
    >>= mapM_ (removeFile . fst)
