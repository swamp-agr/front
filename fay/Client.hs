{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where

import WebSockets

import Bridge
import Client.FFI

import DOM
import Data.Data
import Data.Text ((<>), fromString, pack, Text, splitOn, map)
import Prelude

main :: Fay ()
main = do
  url <- getWsUrl
  ws <- websocket url onOpen (onMessage') no no
  -- FIXME: handle with 'onOpen' instead
  void $ setTimeout 1000 $ \_ -> sendAny ws AskEvents
  where
    no :: WSEvent -> Fay ()
    no = \_ -> return ()

onMessage' :: WSEvent -> Fay ()
onMessage' evt = do
  ws <- target evt
  responseText <- eventData evt
  response <- parse responseText
  case response of
    EmptyCmd -> return ()
    ExecuteClient cid task strategy -> do
      if strategy == ExecuteAll
        then do
          forM_ (executeRenderHtml task) $ \html ->
            case html of
              AttachText eid val -> attachToElemById eid val
              AttachDOM eid val  -> attachToParentById eid val
          forM_ (executeAction task) $ \act -> addListener ws act
        else return ()

addListener :: WebSocket -> CallbackAction (Action a) -> Fay ()
addListener ws (CallbackAction eh) = handle ws eh

handle :: WebSocket -> EventHandler (Action a) -> Fay ()
handle ws eh = case eh of
  OnClick act1 -> handleAction ws act1 onClick
  OnKeyUp act2 -> handleAction ws act2 onKeyUp
  OnValueChange act3 -> handleAction ws act3 onChange
  _ -> log' "not implemented yet"

handleAction :: WebSocket -> Action a -> (Element -> Fay () -> Fay ()) -> Fay ()
handleAction ws (Action e a c) f = do
  elem <- getElementById e
  f elem $ do
    case a of
      RecordAction -> do
        val <- getValue elem
        sendAny ws (Send (Action e a (pushValue c val)))
      ObjectAction -> do
        sendAny ws (Send (Action e a c))

pushValue :: a -> Text -> a
pushValue a _ = a

sendAny :: WebSocket -> a -> Fay ()
sendAny ws val = do
  js <- json val
  sendWS ws js
