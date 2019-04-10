{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Todo where

import           Bridge
import           Control.Concurrent.STM.Lifted     as STM
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


import           Shared
import           Web.Front
import           Web.Front.Broadcast

-- * Model

data Model = Model
  { entries :: [Entry]
  , nextId  :: Int
  }

data Entry = Entry
  { description :: Text
  , eid         :: Int
  }

initModel :: Model
initModel = Model { entries = [], nextId = 0 }

-- * Event Handling

instance CommandHandler TVar Model Msg where
  onCommand cmd stateTVar client = do
    m@Model{..} <- STM.readTVarIO stateTVar
    case readFromFay' cmd of
      Left err -> do
        putStrLn $ "Error: " <> show err
        return (ExecuteClient client emptyTask ExecuteAll)
      Right (Send (Action _ _ acmd)) -> do
        case acmd of
          Add -> do
            let newTodo = Entry "TODO: " nextId
                newState = Model (newTodo : entries) (succ nextId)
                task = createTask "root" renderModel newState
            atomically $ void $ swapTVar stateTVar newState
            return (ExecuteClient client task ExecuteAll)
          Complete _eid -> do
            let newState = Model (filter ((/=_eid) . eid) entries) nextId
                task = createTask "root" renderModel newState
            atomically $ void $ swapTVar stateTVar newState
            return (ExecuteClient client task ExecuteAll)
          Update _eid val -> do
            let newState = Model ((upd _eid val) <$> entries) nextId
                upd _id val' e@Entry{..} =
                  if eid == _id then e { description = val', eid = _id } else e
                task = createTask "root" renderModel newState
            atomically $ void $ swapTVar stateTVar newState
            return (ExecuteClient client task ExecuteExcept)
      Right AskEvents -> do
        let task = createTask "root" renderModel m
        return $ ExecuteClient client task ExecuteAll
      Right PingPong -> return (ExecuteClient client emptyTask ExecuteAll)

-- * Rendering

renderModel :: Model -> H.Markup (Action Msg)
renderModel Model{..} = do
  H.h1 $ "TODO MVC: Servant"
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

parseInt :: Text -> Int
parseInt = toInt . ignoreChars

toInt :: String -> Int
toInt a = read a :: Int

ignoreChars :: Text -> String
ignoreChars = L.filter isDigit . T.unpack

safeFromJust :: forall t. t -> Maybe t -> t
safeFromJust defv Nothing = defv
safeFromJust _ (Just a)   = a
