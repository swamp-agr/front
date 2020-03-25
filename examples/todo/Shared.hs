{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Shared where

import           Prelude

import           Data.Data
import           Data.Text (Text)

type EntryId = Int

data Visibility = All | Active | Completed
  deriving (Show, Typeable, Data, Eq)

data Msg = Msg1 MsgCommon | Msg2 MsgEntry
  deriving (Show, Typeable, Data)

data MsgCommon
  = Add
  | UpdateField { commandValue :: Text }
  | CheckAll Bool
  | DeleteComplete
  | ChangeVisibility Visibility
  deriving (Show, Typeable, Data)

data MsgEntry
  = Editing EntryId Bool
  | UpdateEntry { entryId :: EntryId, commandValue :: Text }
  | Check EntryId Bool
  | Delete EntryId
  deriving (Show, Typeable, Data)
