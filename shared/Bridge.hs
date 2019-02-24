{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
module Bridge where

import Prelude

import Data.Data

-- | One specific and incomplete specifications of event-handlers geared
-- towards their use with JS.
data EventHandler a
    = OnKeyDown   !a
    | OnKeyUp     !a
    | OnKeyPress  !a
    | OnFocus !a
    | OnBlur  !a
    | OnValueChange    !a
    | OnCheckedChange  !a
    | OnSelectedChange !a
    | OnSubmit         !a
    | OnClick       !a
    | OnDoubleClick !a
    | OnMouseDown   !a
    | OnMouseUp     !a
    | OnMouseMove   !a
    | OnMouseEnter  !a
    | OnMouseLeave  !a
    | OnMouseOver   !a
    | OnMouseOut    !a
    | OnScroll !a
    | OnWheel  !a

    -- TODO: Implement these
    -- OnCopy  ([File] -> IO a)
    -- OnCut   ([File] -> IO a)
    -- OnPaste ([File] -> IO a)

    -- TODO: Implement these.
    -- OnDrag      ([File] -> IO a)
    -- OnDragEnd   ([File] -> IO a)
    -- OnDragEnter ([File] -> IO a)
    -- OnDragExit  ([File] -> IO a)
    -- OnDragLeave ([File] -> IO a)
    -- OnDragOver  ([File] -> IO a)
    -- OnDragStart ([File] -> IO a)
    -- OnDrop      ([File] -> IO a)

    -- TODO: add more events.

#ifdef FAY
             deriving (Typeable, Data)
#else
             deriving (Functor, Typeable, Data)
#endif

data CallbackAction a = CallbackAction (EventHandler a)
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Typeable, Data)
#endif
