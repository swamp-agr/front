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
import           Control.Monad                           (unless, when)
import           Control.Monad.Catch                     (catch)
import           Crypto.Random                           (drgNew)
import           Data.Data
import           Data.Default                            (def)
import qualified Data.HashMap.Strict                     as HashMap

import           Data.Text                               (Text)
import           Data.Text.Encoding                      (decodeUtf8)
import           Data.Time                               (defaultTimeLocale,
                                                          formatTime)
import           Data.Time.Clock                         (UTCTime (..),
                                                          getCurrentTime)
import           Network.Wai                             (Application)
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets                      hiding (Headers)
import           Prelude                                 hiding (interact)
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth        (mkAuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           System.Directory                        (createDirectoryIfMissing,
                                                          doesFileExist,
                                                          getModificationTime,
                                                          listDirectory,
                                                          removeFile)
import           System.FilePath.Posix                   ((<.>), (</>))
import           System.Random                           (randomRIO)
import           Text.Blaze.Front.Html5                  ((!))
import           Text.Blaze.Front.Renderer               (renderNewMarkup)
import           Text.Blaze.Html5                        (Html)

import           Control.Concurrent.STM                  as STM
import qualified Data.ByteString.Base64                  as Base64
import qualified Data.ByteString.Char8                   as BSC8
import qualified Data.ByteString.Lazy                    as BL
import qualified Data.List                               as L
import qualified Data.Text                               as T
import qualified Text.Blaze.Front.Html5                  as H
import qualified Text.Blaze.Front.Html5.Attributes       as A

import           Bridge
import           Shared
import           Todo
import           Web.Front.Broadcast

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

-- * API

api :: Proxy API
api = Proxy

server :: Config -> Server API
server cfg = serveRoot cfg :<|> serveStatic cfg
  where
    addSession' = addSession
      (authCookieSettings cfg) -- the settings
      (randomSource cfg)       -- random source
      (serverKeySet cfg)       -- server key set

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
            H.link ! A.rel "stylesheet" ! A.href "/static/todo.css"
          H.body $ do
            H.div ! A.id "root" $ renderModel state)

    serveStatic Config{..} = serveDirectoryWebApp static

addClient :: IO Int
addClient = randomRIO (0, 1000000)


-- * Helpers

checkSession :: Config -> PendingConnection -> IO Int
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
        Nothing -> pure Nothing
        Just cl -> pure $ pure cl
  where
    ex :: AuthCookieExceptionHandler IO
    ex _e = pure Nothing

setSession :: Config -> Int -> IO ()
setSession Config{..} clientId = atomically $ do
  ids <- readTVar clients
  unless (clientId `elem` ids) $ modifyTVar' clients (clientId :)


-- * Main

main :: IO ()
main = do
  let fksp = FileKSParams
        { fkspKeySize = 16
        , fkspMaxKeys = 3
        , fkspPath = "./test-key-set"
        }
  cfg <- Config
    <$> STM.newTVarIO newModel
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
      interact stream writeChan' readChan' model (_client)
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
