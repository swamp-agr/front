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
runWith :: (a -> Text -> a) -> Fay ()
runWith ext = do
  url <- getWsUrl
  ws <- websocket url onOpen (onMessage' ext) no no
  -- FIXME: handle with 'onOpen' instead
  void $ setTimeout 1000 $ \_ -> sendAny ws AskEvents
  where
    no :: WSEvent -> Fay ()
    no = \_ -> return ()

-- | WebSocket 'onMessage' event handler.
onMessage'
  :: (a -> Text -> a) -- ^ Helper function, which used to handle outer effects.
  -> WSEvent -- ^ WebSocket event.
  -> Fay ()
onMessage' transform0 evt = do
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
          forM_ (executeAction task) $ \act -> addListener transform0 ws act
        else return ()

-- | Connect WebSocket with corresponding event handler.
addListener
  :: (a -> Text -> a) -- ^ Helper function, which used to handle outer effects.
  -> WebSocket -- ^ Connection.
  -> CallbackAction (Action a) -- ^ Callback.
  -> Fay ()
addListener transform1 ws (CallbackAction eh) = handle transform1 ws eh

-- | Event handler for incoming action.
-- FIXME: add more events.
handle
  :: (a -> Text -> a) -- ^ Helper function, which used to handle outer effects.
  -> WebSocket -- ^ Connection.
  -> EventHandler (Action a) -- ^ Event Handler is here.
  -> Fay ()
handle transform2 ws eh = case eh of
  OnClick act1       -> handleAction transform2 ws act1 onClick
  OnKeyUp act2       -> handleAction transform2 ws act2 onKeyUp
  OnValueChange act3 -> handleAction transform2 ws act3 onChange
  OnKeyDown act4     -> handleAction transform2 ws act4 onKeyDown
  _x                 -> do
    log' "not implemented yet: "
    log' _x

-- | Default behaviour how to handle incoming commands from server.
-- Based on 'ActionType' corresponding flow chosen.
-- If incoming command has 'ObjectAction' type, then there is expectation to create/delete/make some specific action. Otherwise, some records of data type should be updated/populated, or child element should be changed.
handleAction
  :: (a -> Text -> a) -- ^ update function.
  -> WebSocket -- ^ connection to server
  -> Action a -- ^ wrapped message with instructions how to handle it
  -> (Element -> Fay () -> Fay ()) -- ^ event handler
  -> Fay ()
handleAction transform3 ws (Action e a c) fun = do
  elem <- getElementById e
  let f = fun
  f elem $ do
    case a of
      RecordAction -> do
        val <- getValue elem
        let newVal = pushValue transform3 c val
        sendAny ws (Send (Action e RecordAction newVal))
      ObjectAction -> do
        sendAny ws (Send (Action e ObjectAction c))

-- | json wrapper around message.
sendAny :: WebSocket -> a -> Fay ()
sendAny ws val = do
  js <- json val
  sendWS ws js

pushValue
  :: (a -> Text -> a) -- ^ helper function, how to update value assigned to data.
  -> a -- ^ instance of data type.
  -> Text -- ^ value.
  -> a -- ^ result.
pushValue g initValue1 newValue =
  let exec = g in exec initValue1 newValue
