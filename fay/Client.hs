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

runWith :: (a -> Text -> a) -> Fay ()
runWith ext = do
  url <- getWsUrl
  ws <- websocket url onOpen (onMessage' ext) no no
  -- FIXME: handle with 'onOpen' instead
  void $ setTimeout 1000 $ \_ -> sendAny ws AskEvents
  where
    no :: WSEvent -> Fay ()
    no = \_ -> return ()

onMessage' :: (a -> Text -> a) -> WSEvent -> Fay ()
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

addListener
  :: (a -> Text -> a) -> WebSocket -> CallbackAction (Action a) -> Fay ()
addListener t ws (CallbackAction eh) = handle t ws eh

handle :: (a -> Text -> a) -> WebSocket -> EventHandler (Action a) -> Fay ()
handle tr ws eh = case eh of
  OnClick act1 -> handleAction tr ws act1 onClick
  OnKeyUp act2 -> handleAction tr ws act2 onKeyUp
  OnValueChange act3 -> handleAction tr ws act3 onChange
  _ -> log' "not implemented yet"

handleAction
  :: (a -> Text -> a) -- ^ update function
  -> WebSocket -- ^ connection to server
  -> Action a -- ^ wrapped message with instructions how to handle it
  -> (Element -> Fay () -> Fay ()) -- ^ event handler
  -> Fay ()
handleAction transform1 ws (Action e a c) fun = do
  elem <- getElementById e
  let f = fun
  f elem $ do
    case a of
      RecordAction -> do
        val <- getValue elem
        let newVal = pushValue transform1 c val
        log' newVal
        sendAny ws (Send (Action e RecordAction newVal))
      ObjectAction -> do
        sendAny ws (Send (Action e ObjectAction c))

pushValue
  :: (a -> Text -> a) -> a -> Text -> a
pushValue g initValue1 newValue = let exec = g in exec initValue1 newValue

sendAny :: WebSocket -> a -> Fay ()
sendAny ws val = do
  js <- json val
  sendWS ws js
