{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Shared where

import           Prelude

import           Data.Data
import           Data.Text (Text)
#ifdef FAY
import           Client
#endif
import           Bridge

type EntryId = Int

data Visibility = All | Active | Completed
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data, Eq)
#endif

data Msg = Msg1 MsgCommon | Msg2 MsgEntry
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif

data MsgCommon
  = Add
  | UpdateField Text
  | CheckAll Bool
  | DeleteComplete
  | ChangeVisibility Visibility
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif


data MsgEntry
  = Editing EntryId Bool
  | UpdateEntry EntryId Text
  | Check EntryId Bool
  | Delete EntryId
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif


#ifdef FAY
-- | Boilerplate that teaches client how to assign value into the message.
update :: Msg -> Text -> Msg
-- FIXME: update (Update ix _oldval) newval = Update ix newval
update (Msg1 msgCommon) x = Msg1 (updateMsgCommon msgCommon x)
update (Msg2 msgEntry)  x = Msg2 (updateMsgEntry msgEntry x)

updateMsgCommon :: MsgCommon -> Text -> MsgCommon
updateMsgCommon Add _ = Add
updateMsgCommon (UpdateField _) val2     = UpdateField val2
updateMsgCommon (CheckAll _) val3 = case val3 of
  "checked" -> CheckAll True
  _         -> CheckAll False
updateMsgCommon DeleteComplete _ = DeleteComplete
updateMsgCommon (ChangeVisibility old) val4 = case val4 of
  "All"       -> ChangeVisibility All
  "Active"    -> ChangeVisibility Active
  "Completed" -> ChangeVisibility Completed
  _           -> ChangeVisibility old

updateMsgEntry :: MsgEntry -> Text -> MsgEntry
updateMsgEntry (Editing eid _old) val1 = case val1 of
  "" -> Editing eid False
  _  -> Editing eid True
updateMsgEntry (UpdateEntry eid _) val2 = UpdateEntry eid val2
updateMsgEntry (Check eid _) val3 = case val3 of
  "" -> Check eid False
  _  -> Check eid True
updateMsgEntry (Delete eid) _ = Delete eid

main = runWith update
#endif
