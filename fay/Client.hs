{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
module Client where

import           WebSockets

import           Bridge
import           Client.FFI

import           Data.Data
import           Data.Text  (Text, fromString, map, pack, splitOn, (<>))
import           DOM
import           Prelude

-- | Main function, used as entry point for client.
main :: Fay ()
main = do
  url <- getWsUrl
  ws <- websocket url onOpen (onMessage') no no
  -- FIXME: handle with 'onOpen' instead
  void $ setTimeout 1000 $ \_ -> sendAny ws AskEvents
  where
    no :: WSEvent -> Fay ()
    no = \_ -> return ()

-- | WebSocket 'onMessage' event handler.
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

-- | Connect WebSocket with corresponding event handler.
addListener :: WebSocket -> CallbackAction (Action a) -> Fay ()
addListener ws (CallbackAction eh) = handle ws eh

-- | Event handler for incoming action.
-- FIXME: add more events.
handle :: WebSocket -> EventHandler (Action a) -> Fay ()
handle ws eh = case eh of
  OnClick act1       -> handleAction ws act1 onClick
  OnKeyUp act2       -> handleAction ws act2 onKeyUp
  OnValueChange act3 -> handleAction ws act3 onChange
  _                  -> log' "not implemented yet"

-- | Default behaviour how to handle incoming commands from server.
-- Based on 'ActionType' corresponding flow chosen.
-- If incoming command has 'ObjectAction' type, then there is expectation to create/delete/make some specific action. Otherwise, some records of data type should be updated/populated, or child element should be changed.
handleAction
  :: WebSocket -- ^ connection to server
  -> Action a -- ^ wrapped message with instructions how to handle it
  -> (Element -> Fay () -> Fay ()) -- ^ event handler
  -> Fay ()
handleAction ws (Action e a c) fun = do
  elem <- getElementById e
  let f = fun
  f elem $ do
    case a of
      RecordAction -> do
        val <- getValue elem
        sendAny ws (Send (Action e RecordAction val))
      ObjectAction -> do
        sendAny ws (Send (Action e ObjectAction c))

-- | json wrapper around message.
sendAny :: WebSocket -> a -> Fay ()
sendAny ws val = do
  js <- json val
  sendWS ws js
