#!/usr/bin/env stack
{- stack --resolver lts-12.7 runghc
  --package servant
  --package servant-server
  --package servant-blaze
  --package servant-websockets
  --package text
  --package conduit
  --package time
  --package stm-lifted
  --package bytestring
  ServantTodo
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module ServantTodo where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.STM.Lifted as STM
import Control.Monad (forever, void, forM_)
import Data.Aeson (decode, Value, toJSON)
import Data.Aeson.Text
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import Fay.Convert (readFromFay', showToFay)
import Network.Wai.Handler.Warp
import Network.WebSockets hiding (Headers)
import Prelude hiding (interact)
import Servant
import Servant.API.WebSocket
import Servant.HTML.Blaze
import Servant.Server.StaticFiles
import System.Random (randomRIO)
import Text.Blaze.Front.Html5 ((!), toValue)
import Text.Blaze.Front.Renderer (renderNewMarkup)
import Text.Blaze.Html5 (Html)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.Blaze.Front.Event as E
import qualified Text.Blaze.Front.Html5 as H
import qualified Text.Blaze.Front.Html5.Attributes as A

import Bridge
import Shared
import Web.Front (createTask)

type ServerAPI = Get '[HTML] (Headers '[Header "set-cookie" ByteString] Html)
  :<|> "static" :> Raw
type WebSocketAPI = WebSocket
type API = WebSocketAPI :<|> ServerAPI

-- * App

data Config = Config
  { model :: TVar Model
  , channel :: TChan (Out (Action Msg))
  , static :: FilePath
  , clients :: TVar [Int]
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

-- * API 

initModel :: Model
initModel = Model { entries = [], nextId = 0 }

api :: Proxy API
api = Proxy

server :: Config -> Server API
server cfg = serveSockets cfg :<|> (serveRoot cfg :<|> serveStatic cfg)

-- * Handler for /

serveRoot :: Config -> Handler Html
serveRoot Config{..} = do
  state <- liftIO $ readTVarIO model
  return $ renderNewMarkup $ do
    H.html $ do
      H.head $ do
        H.title "TODO"
        H.script ! A.src "/static/bundle.js" $ ""
      H.body $ do
        H.div ! A.id "root" $ renderModel state

-- serveStatic :: Config -> Handler Raw
serveStatic Config{..} = serveDirectoryWebApp static

serveSockets Config{..} = \stream -> do
  let writeChan' = channel
  readChan' <- atomically $ dupTChan writeChan'
  interact stream writeChan' readChan' model

interact stream in' out' tvar = do
  race_
    (forever $ do
        cmd <- atomically $ readTChan out'
        json <- pure $ toJSON . showToFay $ cmd
        res <- lift $ lookupSession "client"
        case cmd of
          EmptyCmd -> sendTextData stream . toLazyText . encodeToTextBuilder $ json
          ExecuteClient cid task strategy -> do
            case res of
              Nothing -> sendTextData stream . toLazyText . encodeToTextBuilder $ json
              Just sid -> do
                if sid == s2t cid && strategy == ExecuteExcept
                  then do
                    json2 <- pure $ toJSON $ showToFay $
                      ExecuteClient cid task ExecuteExcept
                    sendTextData stream . toLazyText . encodeToTextBuilder $ json2
                  else do
                    json2 <- pure $ toJSON $ showToFay $
                      ExecuteClient cid task ExecuteAll
                    sendTextData stream . toLazyText . encodeToTextBuilder $ json2)

    (sourceWS $$ mapM_C (\cmdstr -> do
      case (decode $ BL.fromChunks [encodeUtf8 cmdstr] :: Maybe Value) of
        Nothing -> error "No JSON provided"
        Just cmd -> do
          liftIO $ putStrLn $ show cmd
          res <- onCommand cmd tvar
          atomically $ writeTChan in' res))
  where
    s2t = T.pack . show
    sourceWS = forever $ lift receiveData >>= yield


-- * Event Handling

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
        Update _eid val -> do
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

clientSession :: Monad m => Config -> m Int
clientSession Config{..} = do
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

main = do
  cfg <- Config
    <$> STM.newTVarIO initModel
    <*> atomically newBroadcastTChan
    <*> pure "./static"
    <*> newTVarIO []
  putStrLn "Server up and running on http://localhost:3000/"
  run 3000 $ serve api (server cfg)
