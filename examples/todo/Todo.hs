{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Todo where

import           Bridge
import           Control.Concurrent.STM            as STM
import           Control.Monad                     (forM_, void)
import           Data.Char                         (isDigit)
import qualified Data.List                         as L
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Fay.Convert                       (readFromFay')
import qualified Text.Blaze.Front.Event            as E
import           Text.Blaze.Front.Html5            (toValue, (!))
import qualified Text.Blaze.Front.Html5            as H
import qualified Text.Blaze.Front.Html5.Attributes as A
--import           Text.Blaze.Html5                  (Html)


import           Web.Front
import           Web.Front.Broadcast


import           Shared

-- * Model

data Model = Model
  { entries    :: [Entry]
  , field      :: Text
  , nextId     :: EntryId
  , visibility :: Visibility
  }

newModel :: Model
newModel = Model
  { entries = []
  , field = ""
  , nextId = 0
  , visibility = All
  }

data Entry = Entry
  { description :: Text
  , completed   :: Bool
  , editing     :: Maybe Text
  , eid         :: EntryId
  }

newEntry :: Text -> EntryId -> Entry
newEntry desc _eid = Entry
  { description = desc
  , completed = False
  , editing = Nothing
  , eid = _eid
  }

-- * Event Handling

instance CommandHandler TVar Model Msg where
  onCommand cmd stateTVar' client = do
    m@Model{..} <- STM.readTVarIO stateTVar'
    case readFromFay' cmd of
      Left err -> do
        putStrLn $ "Error: " <> show err
        return (ExecuteClient client emptyTask ExecuteAll)
      Right (Send (Action _ _ acmd)) -> do
        case acmd of
          Msg1 msgCommon -> onMsgCommon m msgCommon stateTVar' client
          Msg2 msgEntry  -> onMsgEntry m msgEntry stateTVar' client
      Right AskEvents -> do
        let task = createTask "root" renderModel m
        return $ ExecuteClient client task ExecuteAll
      Right PingPong -> return (ExecuteClient client emptyTask ExecuteAll)

onMsgCommon
  :: Model -> MsgCommon -> TVar Model -> ClientId -> IO (Out (Action Msg))
onMsgCommon m@Model{..} msg tvar client = case msg of
  Add -> do
    let update t = t { entries = newEntry "" nextId : entries, nextId = succ nextId }
    go (\t _ -> update t) ()
  UpdateEntry entryId val -> do
    let update v e = e { description = v }
        updateModel t v i f = t { entries = filterMap entries ((== i) . eid) (f v) }
    go (\t v -> updateModel t v entryId update) val
  UpdateField val -> go (\t v -> t { field = v }) val
  CheckAll isChecked -> do
    let update v e = e { completed = v }
    go (\t v -> t { entries = update v <$> entries }) isChecked
  DeleteComplete -> go (\t _ -> t { entries = filter (not . completed) entries } ) ()
  ChangeVisibility v -> go (\t x -> t { visibility = x }) v
  where
    go = pushAll m tvar client

onMsgEntry
  :: Model -> MsgEntry -> TVar Model -> ClientId -> IO (Out (Action Msg))
onMsgEntry m@Model{..} msg tvar client = case msg of
  Focus entryId -> do
    let update v t = t { editing = pure v }
    go (\t v -> updateModel t v entryId update) field
  Cancel entryId -> do
    let update _ t = t { editing = Nothing }
    go (\t v -> updateModel t v entryId update) ()
  Commit entryId -> do
    let update _ t = case editing t of
          Nothing   -> t { description = "" }
          Just desc -> t { description = desc, editing = Nothing }
    go (\t v -> updateModel t v entryId update) ()
  Complete entryId isCompleted -> do
    let update :: Bool -> Entry -> Entry
        update v t = t { completed = v }
    go (\t v -> updateModel t v entryId update) isCompleted
  Delete entryId -> do
    let update e v = e { entries = filter ((/= v) . eid) entries }
    go update entryId
  where
    go = pushAll m tvar client
    updateModel :: Model -> val -> EntryId -> (val -> Entry -> Entry) -> Model
    updateModel t v i f = t { entries = filterMap entries ((== i) . eid) (f v) }

-- * Helpers

pushExcept
  :: state
  -> TVar Model
  -> ClientId
  -> (state -> val -> Model)
  -> val
  -> IO (Out (Action Msg))
pushExcept model tvar client f val =
  push model tvar client f val ExecuteExcept

pushAll
  :: state
  -> TVar Model
  -> ClientId
  -> (state -> val -> Model)
  -> val
  -> IO (Out (Action Msg))
pushAll model tvar client f val =
  push model tvar client f val ExecuteAll

push
  :: state
  -> TVar Model
  -> ClientId
  -> (state -> val -> Model)
  -> val
  -> ExecuteStrategy
  -> IO (Out (Action Msg))
push model tvar client f val execStrategy = do
  let newState = f model val
      task = createTask "root" renderModel newState
  atomically $ void $ swapTVar tvar newState
  pure (ExecuteClient client task execStrategy)

-- * Rendering

renderModel :: Model -> H.Markup (Action Msg)
renderModel Model{..} = do
  H.div ! A.class_ "todomvc-wrapper" $ do
    H.section ! A.class_ "todoapp" $ do
      H.header ! A.class_ "header" $ do
        H.h1 $ "todos"
        H.input
          ! A.class_ "new-todo"
          ! A.id "new-todo"
          ! A.placeholder "What needs to be done?"
          ! A.autofocus ""
          ! E.onKeyDown (Action "new-todo" RecordAction (Msg1 $ UpdateField ""))
          ! A.name "newTodo"

      H.section ! A.class_ "main" ! A.style "visibility: hidden;" $ do
        H.input ! A.class_ "toggle-all" ! A.type_ "checkbox"
          ! A.name "toggle"
        H.label ! A.for "toggle-all" $ "Mark all as complete"
        H.ul ! A.class_ "todo-list" $ do
          forM_ entries $ \entry -> renderEntry entry
      H.footer ! A.class_ "footer" ! A.hidden "hidden" $ do
        H.span ! A.class_ "todo-count" $ do
          H.strong "0"
          H.span $ " items left"

        H.ul ! A.class_ "filters" $ do
          H.li ! A.class_ "selected" $ "All"
          H.li ! A.class_ "" $ "Active"
          H.li ! A.class_ "" $ "Completed"
    H.footer ! A.class_ "info" $ do
      H.p $ "Double-click to edit todo"
      H.p $ do
        H.span $ "Written by "
        H.a ! A.href "https://github.com/swamp-agr" $ "Andrey Prokopenko"
      H.p $ do
        H.span "Part of "
        H.a ! A.href "https://todomvc.com" $ "TODO MVC"

renderEntry :: Entry -> H.Markup (Action Msg)
renderEntry Entry{..} = do
  let elemId = "todo-" <> (T.pack $ show eid)
      removeId = "remove-" <> (T.pack $ show eid)
  H.input
    ! A.id (toValue elemId)
    ! A.type_ "text"
    ! A.value (toValue description)
  H.button
    ! A.id (toValue removeId)
    ! A.type_ "submit"
    $ "Done"
  H.br

-- * Helpers

parseInt :: Text -> Int
parseInt = toInt . ignoreChars

toInt :: String -> Int
toInt a = read a :: Int

ignoreChars :: Text -> String
ignoreChars = L.filter isDigit . T.unpack

safeFromJust :: forall t. t -> Maybe t -> t
safeFromJust defv Nothing = defv
safeFromJust _ (Just a)   = a

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs f g = (\x -> if f x then g x else x) <$> xs
