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

main :: Fay ()
main = runWith

-- | Main function, used as entry point for client.
runWith :: {-(a -> Text -> a) ->-} Fay ()
runWith {-ext-} = do
  url <- getWsUrl
  ws <- websocket url onOpen (onMessage' {-ext-}) no no
  -- FIXME: handle with 'onOpen' instead
  void $ setTimeout 1000 $ \_ -> sendAny ws AskEvents
  void $ setInterval 30000 $ \_ -> sendAny ws PingPong
  return ()
  where
    no :: WSEvent -> Fay ()
    no = \_ -> return ()

-- | WebSocket 'onMessage' event handler.
onMessage'
  :: WSEvent -- ^ WebSocket event.
  -> Fay ()
onMessage' {-transform0-} evt = do
  ws <- target evt
  responseText <- eventData evt
  response <- parse responseText
  handleResponse ws {-transform0-} response

handleResponse :: WebSocket -> {-(a -> Text -> a) ->-} Out (Action a) -> Fay ()
handleResponse _ws {-_helper-} EmptyCmd = return ()
handleResponse ws {-helper-} (ExecuteClient cid task strategy) = do
  if strategy == ExecuteAll
    then do
      forM_ (executeRenderHtml task) $ handleRenderHtml
      forM_ (executeAction task) $ \callback -> addListener ws {-helper-} callback
    else return ()

handleRenderHtml :: RenderHtml -> Fay ()
handleRenderHtml (AttachText eid1 val1) = attachToElemById eid1 val1
handleRenderHtml (AttachDOM eid2 val2)  = attachToParentById eid2 val2

-- | Connect WebSocket with corresponding event handler.
addListener
  :: WebSocket -- ^ Connection.
  {--> (a -> Text -> a) -- ^ Helper function, which used to handle outer effects.-}
  -> CallbackAction (Action a) -- ^ Callback.
  -> Fay ()
addListener ws {-transform1-} (CallbackAction eh) = handle {-transform1-} ws eh

-- | Event handler for incoming action.
-- FIXME: add more events.
handle
  :: {-(a -> Text -> a) -- ^ Helper function, which used to handle outer effects.
  ->-} WebSocket -- ^ Connection.
  -> EventHandler (Action a) -- ^ Event Handler is here.
  -> Fay ()
handle {-transform2-} ws eh = case eh of
  OnClick act1       -> handleAction {-transform2-} ws act1 onClick
  OnKeyUp act2       -> handleAction {-transform2-} ws act2 onKeyUp
  OnValueChange act3 -> handleAction {-transform2-} ws act3 onChange
  OnKeyDown act4     -> handleAction {-transform2-} ws act4 onKeyDown
  OnKeyPress act5    -> handleAction {-transform2-} ws act5 onKeyPress
  OnEnter act6       -> handleAction {-transform2-} ws act6 onEnter
  OnBlur act7        -> handleAction {-transform2-} ws act7 onBlur
  OnDoubleClick act8 -> handleAction {-transform2-} ws act8 onDoubleClick
  _x                 -> do
    log' "not implemented yet: "
    log' _x

-- | Default behaviour how to handle incoming commands from server.
-- Based on 'ActionType' corresponding flow chosen.
-- If incoming command has 'ObjectAction' type, then there is expectation to create/delete/make some specific action. Otherwise, some records of data type should be updated/populated, or child element should be changed.
handleAction
  :: {-(a -> Text -> a) -- ^ update function.
  ->-} WebSocket -- ^ connection to server
  -> Action a -- ^ wrapped message with instructions how to handle it
  -> (Element -> (a -> Fay ()) -> Fay ()) -- ^ event handler
  -> Fay ()
handleAction {-transform3-} ws (Action e a c) fun = do
  elem <- getElementById e
  let f = fun
  f elem $ \evt -> do
    case a of
      EnterAction -> do
        code <- keyCode evt
        log' code
        case code of
          13 -> do
            log' "we are here"
            sendAny ws (Send (Action e EnterAction c))
          _ -> return ()

      RecordAction -> do
        val <- value elem
        newC <- pushValue {-transform3-} c val
        sendAny ws $! (Send (Action e RecordAction newC))

      ObjectAction -> do
        sendAny ws (Send (Action e ObjectAction c))

-- | json wrapper around message.
sendAny :: WebSocket -> a -> Fay ()
sendAny ws val = do
  js <- json val
  sendWS ws js

pushValue
  :: a -- ^ instance of data type. -- (a -> Text -> a) -- ^ helper function, how to update value assigned to data.
  -> Text -- ^ value.
  -> Fay a -- ^ result.
pushValue initValue1 newValue = do
  way <- findProperty initValue1 "commandValue"
  newItemText <- setProperty initValue1 way newValue
  newItem <- parse newItemText
  return $! newItem
