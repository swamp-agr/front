{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
module Bridge where

import Prelude

import Data.Data
import Data.Text (Text)


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

data IncomingCommand a = PingPong
  | Send (Action a)
  | AskEvents
  deriving (Data, Typeable)

data OutcomingCommand a = EmptyCmd
  | ExecuteClient ClientId (ClientTask a) ExecuteStrategy
  deriving (Data, Typeable)

data ClientTask a = ClientTask
  { executeRenderHtml :: [RenderHtml]
  , executeAction :: [CallbackAction (Action a)]
  } deriving (Data, Typeable)

data RenderHtml = AttachText ElementId HtmlText
  | AttachDOM ElementId HtmlText deriving (Data, Typeable)

data CallbackAction a = CallbackAction (EventHandler a)
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Typeable, Data)

instance Show a => Show (CallbackAction a) where
  show = show
#endif

data Action a = Action ElementId ActionType a
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif

elementId :: Action a -> ElementId
elementId (Action e _ _) = e

actionType :: Action a -> ActionType
actionType (Action _ a _) = a

actionCmd :: Action a -> a
actionCmd (Action _ _ c) = c

updateAction :: Action a -> a -> Action a
updateAction (Action e a _) c = Action e a c

data ActionType = RecordAction | ObjectAction
#ifdef FAY
  deriving (Typeable, Data)
#else
  deriving (Show, Typeable, Data)
#endif

data ExecuteStrategy =
  ExecuteAll | ExecuteExcept
  deriving (Data, Typeable, Eq)

type ElementId = Text

type HtmlText = Text

type ObjectId = Int

type AttrId = Int

type ClientId = Int

type RowNumber = Int

type RecordValue = Text

-- | Pretty-printer for command expected from Client.
ppIncomingCommand :: IncomingCommand a -> Text
ppIncomingCommand AskEvents = "AskEvents"
ppIncomingCommand (Send _) = "SendObjectAction"
ppIncomingCommand PingPong = "PingPong"
