#!/usr/bin/env stack
{- stack --resolver lts-12.7 runghc
  --package yesod-core
  --package yesod-websockets
  --package text
  --package conduit
  --package time
  --package stm-lifted
  --package bytestring
  Main
-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, NoImplicitPrelude, DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
module Main where

import Conduit
import Control.Monad (forever, void, forM_)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.Lifted as STM
import Data.Aeson (decode)
import Data.Aeson.Text
import Data.Char (isDigit)
import Data.Conduit
import Data.Data
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time
import Text.Blaze.Front.Html5 ((!), toValue)
import Text.Blaze.Front.Renderer (renderNewMarkup)
import Fay.Convert (readFromFay', showToFay)
import Prelude hiding (interact)
import System.Random (randomRIO)
import Yesod.Core
import Yesod.Static
import Yesod.WebSockets

import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Front.Event as E
import qualified Text.Blaze.Front.Html5 as H
import qualified Text.Blaze.Front.Html5.Attributes as A
import qualified Text.Blaze.Front.Renderer as H

import Bridge
import Web.Front (createTask)

-- * App

data App = App
  { appModel :: TVar Model
  , appChannel :: TChan (Out (Action Msg))
  , appStatic :: Static
  }

-- * Model

data Model = Model
  { entries :: [Entry]
  , nextId :: Int
  }

data Entry = Entry
  { description :: Text
  , eid :: Int
  }

-- * Actions

data Msg = UpdateEntry Int Bool
  | Add
  | Complete Int
  | Update Int RecordValue
  deriving (Show, Typeable, Data)

initModel :: Model
initModel = Model { entries = [], nextId = 0 }

mkYesod "App" [parseRoutes|
/ HomeR GET
/static      StaticR  Static  appStatic
|]

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent [whamlet|^{widget}|]
    withUrlRenderer [hamlet|<html>
  <head>
    ^{pageHead pc}
    <script src="/static/fay-runtime.js">
    <script src="/static/bundle.js">
  <body>
    <div #root>^{pageBody pc}
|]

-- * Handler for /

getHomeR :: Handler Html
getHomeR = do
  modelTVar <- appModel <$> getYesod
  model <- STM.readTVarIO modelTVar
  webSockets $ webApp modelTVar
  defaultLayout $ do
    setTitle "TODO"
    toWidget $ renderNewMarkup $ renderModel model

-- * WebSockets Web application

webApp tvar = do
  writeChan' <- appChannel <$> getYesod
  readChan' <- atomically $ do
    dupTChan writeChan'
  interact writeChan' readChan' tvar

interact
    :: TChan (Out (Action Msg))
    -> TChan (Out (Action Msg))
    -> TVar Model
    -> WebSocketsT Handler ()
interact in' out' tvar = do
  race_
    (forever $ do
        cmd <- atomically $ readTChan out'
        json <- returnJson . showToFay $ cmd
        res <- lift $ lookupSession "client"
        case cmd of
          EmptyCmd -> sendTextData . toLazyText . encodeToTextBuilder $ json
          ExecuteClient cid task strategy -> do
            case res of
              Nothing -> sendTextData . toLazyText . encodeToTextBuilder $ json
              Just sid -> do
                if sid == s2t cid && strategy == ExecuteExcept
                  then do
                    json2 <- returnJson $ showToFay $
                      ExecuteClient cid task ExecuteExcept
                    sendTextData . toLazyText . encodeToTextBuilder $ json2
                  else do
                    json2 <- returnJson $ showToFay $
                      ExecuteClient cid task ExecuteAll
                    sendTextData . toLazyText . encodeToTextBuilder $ json2)

    (sourceWS $$ mapM_C (\cmdstr -> do
      case (decode $ BL.fromChunks [encodeUtf8 cmdstr] :: Maybe Value) of
        Nothing -> error "No JSON provided"
        Just cmd -> do
          liftIO $ putStrLn $ show cmd
          res <- onWSCommand cmd tvar
          atomically $ writeTChan in' res))
  where
    s2t = T.pack . show

-- * Event Handling

onWSCommand
  :: Value -> TVar Model -> WebSocketsT (HandlerFor App) (Out (Action Msg))
onCommand cmd stateTVar = do
  m@Model{..} <- STM.readTVarIO stateTVar
  case readFromFay' cmd of
    Right (Send (Action _ _ acmd)) -> do
      case acmd of
        Add -> do
          let newTodo = Entry "TODO: " nextId
              newState = Model (newTodo : entries) (succ nextId)
              task = createTask "root" renderModel newState
          atomically $ void $ swapTVar stateTVar newState
          cid <- lift clientSession
          return (ExecuteClient cid task ExecuteAll)
        Complete _eid -> do
          let newState = Model (filter ((/=_eid) . eid) entries) nextId
              task = createTask "root" renderModel newState
          atomically $ void $ swapTVar stateTVar newState
          cid <- lift clientSession
          return (ExecuteClient cid task ExecuteAll)
        Update _eid val-> do
          let newState = Model ((upd _eid val) <$> entries) nextId
              upd _id val' e@Entry{..} =
                if eid == _id then e { description = val', eid = _id } else e
              task = createTask "root" renderModel newState
          atomically $ void $ swapTVar stateTVar newState
          cid <- lift clientSession
          return (ExecuteClient cid task ExecuteExcept)
    Right AskEvents -> do
      let task = createTask "root" renderModel m
      cid <- lift clientSession
      return $ ExecuteClient cid task ExecuteAll

-- * Rendering

renderModel :: Model -> H.Markup (Action Msg)
renderModel Model{..} = do
  H.h1 $ "TODO MVC"
  H.br
  forM_ entries $ \entry -> renderEntry entry
  let btnId = "todo-add"
  H.button
    ! A.id "todo-add"
    ! A.type_ "submit"
    ! E.onClick (Action btnId ObjectAction Add)
    $ "Добавить"

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
    $ "Завершить"
  H.br

-- * Helpers 

clientSession :: MonadHandler m => m Int
clientSession = do
  res <- lookupSession "client"
  case res of
    Nothing -> do
      (rnd :: Int) <- liftIO $ randomRIO (0,200000)
      let clientid = T.pack . show $ rnd
      setSession "client" clientid
      return rnd
    Just cid -> return $ parseInt cid

parseInt :: Text -> Int
parseInt = toInt . ignoreChars

toInt :: String -> Int
toInt a = read a :: Int

ignoreChars :: Text -> String
ignoreChars = L.filter isDigit . T.unpack

safeFromJust :: forall t. t -> Maybe t -> t
safeFromJust defv Nothing = defv
safeFromJust _ (Just a) = a


-- * Main

main :: IO ()
main = do
  (App
    <$> STM.newTVarIO initModel
    <*> atomically newBroadcastTChan
    <*> staticDevel "./static")
  >>= warp 3000
