{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
module Shared where

import Prelude

import Data.Data
import Data.Text (Text)
#ifdef FAY
import Client
#endif
import Bridge

data Msg = Add
  | Complete Int
  | Update Int RecordValue
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif

update :: Msg -> Text -> Msg
update (Update ix _oldval) newval = Update ix newval
update x _ = x

#ifdef FAY
main = runWith update
#endif
