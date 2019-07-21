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
  , editing     :: Bool
  , eid         :: EntryId
  }

newEntry :: Text -> EntryId -> Entry
newEntry desc _eid = Entry
  { description = desc
  , completed = False
  , editing = False
  , eid = _eid
  }

-- * Event Handling

instance CommandHandler TVar Model Msg where
  onCommand cmd stateTVar' client = do
    putStrLn (show cmd)
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
    let update t =
          t { entries = newEntry field nextId : entries, nextId = succ nextId, field = "" }
    go (\t _ -> update t) ()
  UpdateField val -> go2 (\t v -> t { field = v }) val
  CheckAll isChecked -> do
    let update v e = e { completed = v }
    go (\t v -> t { entries = update v <$> entries }) isChecked
  DeleteComplete ->
    go (\t _ -> t { entries = filter (not . completed) entries } ) ()
  ChangeVisibility v -> go (\t x -> t { visibility = x }) v
  where
    go = pushAll m tvar client
    go2 = pushExcept m tvar client

onMsgEntry
  :: Model -> MsgEntry -> TVar Model -> ClientId -> IO (Out (Action Msg))
onMsgEntry m@Model{..} msg tvar client = case msg of
  Editing entryId val -> do
    let update v t = t { editing = v }
    go (\t v -> updateModel t v entryId update) val
  Check entryId val -> do
    let update v t = t { completed = v }
    go (\t v -> updateModel t v entryId update) val
  UpdateEntry entryId val -> do
    let update desc t = t { description = desc }
    go2 (\t v -> updateModel t v entryId update) val
  Delete entryId -> do
    let update e v = e { entries = filter ((/= v) . eid) entries }
    go update entryId
  where
    go = pushAll m tvar client
    go2 = pushExcept m tvar client
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
          ! A.value (toValue field)
          ! E.onEnter (Action "new-todo" EnterAction (Msg1 Add))
          ! E.onKeyPress (Action "new-todo" RecordAction (Msg1 $ UpdateField field))
          ! A.name "newTodo"

      renderEntries visibility entries
      renderControls visibility entries

    H.footer ! A.class_ "info" $ do
      H.p $ "Double-click to edit todo"
      H.p $ do
        H.span $ "Written by "
        H.a ! A.href "https://github.com/swamp-agr" $ "Andrey Prokopenko"
      H.p $ do
        H.span "Part of "
        H.a ! A.href "https://todomvc.com" $ "TODO MVC"

renderEntries :: Visibility -> [Entry] -> H.Markup (Action Msg)
renderEntries visibility entries = do
  H.section
    ! A.class_ "main" ! A.style ("visibility: " <> cssVisibility entries <> ";")
  $ do H.input
         ! A.class_ "toggle-all"
         ! A.type_ "checkbox"
         ! A.name "toggle"
         ! checkIfTrue allCompleted
       H.label ! A.for "toggle-all" $ "Mark all as complete"
       H.ul ! A.class_ "todo-list" $ do
         forM_ (filter isVisible entries) $ \entry -> renderEntry entry
  where
    cssVisibility [] = "hidden"
    cssVisibility _  = "visible"

    allCompleted = all (==True) $ completed <$> entries
    isVisible Entry{..} =
      case visibility of
        Completed -> completed
        Active    -> not completed
        _         -> True

renderEntry :: Entry -> H.Markup (Action Msg)
renderEntry Entry{..} = do
  let elemId = "todo-" <> tid
      labelId = "label-" <> tid
      checkId = "check-" <> tid
      removeId = "remove-" <> tid
      tid = T.pack $ show eid
  H.li
    ! A.class_ (toValue $ T.intercalate " " $
                 [ "completed" | completed ] <> [ "editing" | editing ])
    $ do
        H.div ! A.class_ "view" $ do
          H.input
            ! A.class_ "toggle"
            ! A.id (toValue checkId)
            ! A.type_ "checkbox"
            ! checkIfTrue completed
            ! A.value ""
            ! E.onClick
                (Action checkId ObjectAction (Msg2 $ Check eid (not completed)))
          H.label
            ! A.id (toValue labelId)
            ! E.onDoubleClick
                (Action labelId ObjectAction (Msg2 $ Editing eid True))
            $ H.toHtml description
          H.button
            ! A.id (toValue removeId)
            ! A.class_ "destroy"
            ! A.type_ "submit"
            ! E.onClick
                (Action removeId ObjectAction (Msg2 $ Delete eid))
            $ ""
        H.input
          ! A.class_ "edit"
          ! A.value (toValue description)
          ! A.name "title"
          ! A.id (toValue elemId)
          ! E.onValueChange
              (Action elemId RecordAction (Msg2 $ UpdateEntry eid description))
          ! E.onBlur
              (Action elemId ObjectAction (Msg2 $ Editing eid False))
          ! E.onEnter
              (Action elemId EnterAction (Msg2 $ Editing eid False))

  H.br

renderControls :: Visibility -> [Entry] -> H.Markup (Action Msg)
renderControls visibility entries = do
  H.footer ! A.class_ "footer" ! hideIfTrue (null entries) $ do
    H.span ! A.class_ "todo-count" $ do
      H.strong (H.toHtml $ show entriesLeft)
      H.span $ H.toHtml
        (" item" <> (if entriesLeft == 1 then "" else "s") <> " left" :: String)

    H.ul ! A.class_ "filters" $ do
      swapVisibility "#/" All visibility
      swapVisibility "#/active" Active visibility
      swapVisibility "#/completed" Completed visibility

    H.button ! A.class_ "clear-completed"
      ! A.id "clear-completed"
      ! hideIfTrue (entriesCompleted == 0)
      ! E.onClick (Action "clear-completed" ObjectAction  (Msg1 DeleteComplete))
      $ H.toHtml ("Clear completed (" <> show entriesCompleted <> ")")

  where
    entriesLeft = length entries - entriesCompleted
    entriesCompleted = length . filter completed $ entries
    swapVisibility :: Text -> Visibility -> Visibility -> H.Markup (Action Msg)
    swapVisibility uri visibility' actualVisibility = H.li $ do
      let uid = T.pack $ show visibility'
      H.a ! A.href (toValue uri)
          ! A.id (toValue uid)
          ! A.class_ (toValue $ T.concat [ "selected" | visibility' == actualVisibility ])
          ! E.onClick
              (Action uid ObjectAction (Msg1 $ ChangeVisibility visibility'))
          $ H.toHtml uid

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

hideIfTrue :: Bool -> (H.Attribute (Action Msg))
hideIfTrue predicate = if predicate
  then A.hidden ""
  else A.attribute "" "" ""

checkIfTrue :: Bool -> (H.Attribute (Action Msg))
checkIfTrue predicate = if predicate
  then A.checked "checked"
  else A.attribute "" "" ""
