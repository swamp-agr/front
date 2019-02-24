{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A preliminary renderer that produces `JS` components when run using
-- Fay.
--
module Text.Blaze.Front.Renderer where

import qualified Data.ByteString.Char8 as SBC
import           Data.List             (isInfixOf)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.ByteString       as S
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy         as TL

import           Prelude               hiding (span)

import           Text.Blaze.Front
import           Text.Blaze.Front.Internal

-- import qualified Text.Blaze as B
import qualified Text.Blaze.Html as B

import           Bridge

------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------


-- | Escape predefined XML entities in a text value
--
escapeMarkupEntities :: Text     -- ^ Text to escape
                   -> Builder  -- ^ Resulting text builder
escapeMarkupEntities = T.foldr escape mempty
  where
    escape :: Char -> Builder -> Builder
    escape '<'  b = TLB.fromText "&lt;"   `mappend` b
    escape '>'  b = TLB.fromText "&gt;"   `mappend` b
    escape '&'  b = TLB.fromText "&amp;"  `mappend` b
    escape '"'  b = TLB.fromText "&quot;" `mappend` b
    escape '\'' b = TLB.fromText "&#39;"  `mappend` b
    escape x    b = TLB.singleton x       `mappend` b

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> Builder        -- ^ String to append
                 -> Builder        -- ^ Resulting string
fromChoiceString (Static s)     = (((TLB.fromText . getText) s) `mappend`)
fromChoiceString (String s)     = (((escapeMarkupEntities . T.pack) s) `mappend`)
fromChoiceString (Text s)       = ((escapeMarkupEntities s) `mappend`)
fromChoiceString (ByteString s) = (((TLB.fromText . T.pack . SBC.unpack) s) `mappend`)
fromChoiceString (PreEscaped x) =
    case x of
      String s -> (((TLB.fromText . T.pack) s) `mappend`)
      Text   s -> ((TLB.fromText s) `mappend`)
      s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (((TLB.fromText . T.pack) s) `mappend`)
    Text   s     -> if "</" `T.isInfixOf` s then id else ((TLB.fromText s) `mappend`)
    ByteString s -> if "</" `S.isInfixOf` s then id else (((TLB.fromText . T.pack . SBC.unpack) s) `mappend`)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id


-- | Render some 'Markup' to a virtual dom.
--
-- This function is morally pure.
--
render
    :: Show act
    => Markup act
    -> Builder
    -> Builder
render = go 0 id
  where
    go :: forall act' b. Int
      -> (Builder -> Builder)
      -> MarkupM act' b
      -> Builder -> Builder
    go i attrs (Parent _ open close content) =
        ind i
        . (((TLB.fromText . getText) open) `mappend`)
        . attrs . ((TLB.fromText ">\n") `mappend`)
        . go (inc i) id content . ind i
        . (((TLB.fromText . getText) close) `mappend`)
        .  ((TLB.singleton '\n') `mappend`)
    go i attrs (CustomParent tag content) =
        ind i
        . ((TLB.singleton '<') `mappend`)
        . fromChoiceString tag . (attrs)
        . ((TLB.fromText ">\n") `mappend`)
        . go (inc i) id content . ind i
        . ((TLB.fromText "</") `mappend`)
        . fromChoiceString tag
        . ((TLB.fromText ">\n") `mappend`)
    go i attrs (Leaf _ begin end) =
        ind i
        . (((TLB.fromText . getText) begin) `mappend`)
        . (attrs)
        . (((TLB.fromText . getText) end) `mappend`)
        . ((TLB.singleton '\n') `mappend`)
    go i attrs (CustomLeaf tag close) =
        ind i
        . ((TLB.singleton '<') `mappend`)
        . fromChoiceString tag . attrs
        . ((TLB.fromText (if close then " />\n" else ">\n")) `mappend`)
    go i attrs (AddAttribute _ key value h) = flip (go i) h $
        (((TLB.fromText . getText) key) `mappend`)
        . fromChoiceString value
        . ((TLB.singleton '"') `mappend`) . attrs
    go i attrs (AddCustomAttribute key value h) = flip (go i) h $
        ((TLB.singleton ' ') `mappend`)
        . fromChoiceString key
        . ((TLB.fromText "=\"") `mappend`)
        . fromChoiceString value
        . ((TLB.singleton '"') `mappend`) .  attrs
    go i _ (Content content) = ind i . fromChoiceString content
        . ((TLB.singleton '\n') `mappend`)
    go i attrs (Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ (Empty) = id
    go _ _ (MapActions _ _) = id
    go i attrs (OnEvent _ h) = go i attrs h  -- will be registered later through registerEvent
    {-# NOINLINE go #-}

    -- Increase the indentation
    inc = (+) 4

    -- Produce appending indentation
    ind i = ((TLB.fromString (replicate i ' ')) `mappend`)
{-# INLINE render #-}

renderHtml
    :: Show act
    => Markup act
    -> String
renderHtml html = TL.unpack . TLB.toLazyText $ render html TLB.flush
{-# INLINE renderHtml #-}

------------------------------------------------------------------------------
-- Event handler callback construction
------------------------------------------------------------------------------

-- | JS defines the following event types:
data EventType
      -- Clipboard Events
    = OnCopyE | OnCutE | OnPasteE
      -- Keyboard Events
    | OnKeyDownE | OnKeyPressE | OnKeyUpE
      -- Focus Events
    | OnFocusE | OnBlurE
      -- Form Events
    | OnChangeE | OnInputE | OnSubmitE
      -- Mouse Events
    | OnClickE | OnDoubleClickE | OnDragE | OnDragEndE | OnDragEnterE
    | OnDragExitE | OnDragLeaveE | OnDragOverE | OnDragStartE | OnDropE
    | OnMouseDownE | OnMouseEnterE | OnMouseLeaveE | OnMouseMoveE
    | OnMouseOutE | OnMouseOverE | OnMouseUpE
      -- Touch Events
    | OnTouchCancelE | OnTouchEndE | OnTouchMoveE | OnTouchStartE
      -- UI Events
    | OnScrollE
      -- Wheel Events
    | OnWheelE

eventName :: EventType -> String
eventName _ = ""
{-eventName ev = case ev of
    OnCopyE        -> "onCopy"
    OnCutE         -> "onCut"
    OnPasteE       -> "onPaste"
    OnKeyDownE     -> "onKeyDown"
    OnKeyPressE    -> "onKeyPress"
    OnKeyUpE       -> "onKeyUp"
    OnFocusE       -> "onFocus"
    OnBlurE        -> "onBlur"
    OnChangeE      -> "onChange"
    OnInputE       -> "onInput"
    OnSubmitE      -> "onSubmit"
    OnClickE       -> "onClick"
    OnDoubleClickE -> "onDoubleClick"
    OnDragE        -> "onDrag"
    OnDragEndE     -> "onDragEnd"
    OnDragEnterE   -> "onDragEnter"
    OnDragExitE    -> "onDragExit"
    OnDragLeaveE   -> "onDragLeave"
    OnDragOverE    -> "onDragOver"
    OnDragStartE   -> "onDragStart"
    OnDropE        -> "onDrop"
    OnMouseDownE   -> "onMouseDown"
    OnMouseEnterE  -> "onMouseEnter"
    OnMouseLeaveE  -> "onMouseLeave"
    OnMouseMoveE   -> "onMouseMove"
    OnMouseOutE    -> "onMouseOut"
    OnMouseOverE   -> "onMouseOver"
    OnMouseUpE     -> "onMouseUp"
    OnTouchCancelE -> "onTouchCancel"
    OnTouchEndE    -> "onTouchEnd"
    OnTouchMoveE   -> "onTouchMove"
    OnTouchStartE  -> "onTouchStart"
    OnScrollE      -> "onScroll"
    OnWheelE       -> "onWheel"-}

data Handler
    = IgnoreEvent
    | HandleEvent (IO (Bool -> IO ()))
      -- ^ Contains an IO action which generates the callback to attach to the event

registerEvents
    :: Markup a -> [CallbackAction a] -> [CallbackAction a]
registerEvents x = go x
  where
    go :: MarkupM a b -> [CallbackAction a] -> [CallbackAction a]
    go (MapActions _ _) = id
    go (Parent _ _ _ content) = go content
    go (CustomParent _ content) = go content
    go (Leaf _ _ _) = id
    go (CustomLeaf _ _) = id
    go (Content _) = id
    go (Append a b) = (go a) . (go b)
    go (AddAttribute _ _ _ a) = go a
    go (AddCustomAttribute _ _ a) = go a
    go Empty = id
    go (OnEvent eh a) = ((reg eh) :) . (go a)

    reg x' = CallbackAction x'

renderNewMarkup :: Show act => Markup act -> B.Html
renderNewMarkup = B.preEscapedToHtml . T.pack . renderHtml

