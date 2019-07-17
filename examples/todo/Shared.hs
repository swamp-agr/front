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
  deriving (Show, Typeable, Data)
#endif

data Msg = Msg1 MsgCommon | Msg2 MsgEntry
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif

data MsgCommon
  = Add
  | UpdateEntry EntryId Text
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
  = Focus EntryId
  -- \| Edit EntryId Text
  | Cancel EntryId
  | Commit EntryId
  | Complete EntryId Bool
  | Delete EntryId
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif


#ifdef FAY
update :: Msg -> Text -> Msg
-- FIXME: update (Update ix _oldval) newval = Update ix newval
update x _                        = x

main = runWith update
#endif
