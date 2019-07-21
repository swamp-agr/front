{-# LANGUAGE DeriveFunctor #-}

module Text.Blaze.Front.Event
    ( -- * Event handling
      mapActions

      -- ** Keyboard events
    , onKeyDown
    , onKeyUp
    , onKeyPress
    , onEnter

      -- ** Focus events
    , onFocus
    , onBlur

      -- ** Form events
    , onValueChange
    , onCheckedChange
    , onSelectedChange
    , onSubmit

      -- ** Mouse events
    , onClick
    , onDoubleClick
    , onMouseDown
    , onMouseUp
    , onMouseMove
    , onMouseEnter
    , onMouseLeave
    , onMouseOver
    , onMouseOut

      -- ** UI Events
    , onScroll

      -- ** Wheel Events
    , onWheel

    ) where

import           Prelude
import           Text.Blaze.Front.Internal (Attribute (..), Markup,
                                            MarkupM (..))

import           Bridge

-- | Modify all event handlers attached to a 'Markup' tree so that the given
-- function is applied to the return values of their callbacks.
mapActions :: (act -> act') -> Markup act -> Markup act'
mapActions = MapActions

-- Keyboard events
-------------------------------------------------------------------------------

-- | The user has pressed a physical key while the target element was focused.
onKeyDown :: act -> Attribute act
onKeyDown = onEvent . OnKeyDown

-- | The user has released a phyiscal key while the target element was focused.
onKeyUp :: act -> Attribute act
onKeyUp = onEvent . OnKeyUp

-- | The user has input some ASCII character while the target element was focused.
onKeyPress :: act -> Attribute act
onKeyPress = onEvent . OnKeyPress

-- | The user has pressed <Enter> while the target element was focused.
onEnter :: act -> Attribute act
onEnter = onEvent . OnEnter

-- Focus events
-------------------------------------------------------------------------------

-- | The focus has moved to the target element.
onFocus :: act -> Attribute act
onFocus = onEvent . OnFocus

-- | The focus has left the target element.
onBlur :: act -> Attribute act
onBlur = onEvent . OnBlur

-- Form events
-------------------------------------------------------------------------------

-- | The 'value' property of the target element has changed. The new value is
-- passed as a parameter to the callback. This handler is supported for
-- <input>, <textarea>, and <select> elements.
onValueChange :: act -> Attribute act
onValueChange = onEvent . OnValueChange

-- | The 'checked' property of the target element has changed. This handler is
-- supported for <input> elements of type 'checkbox' or 'radio'.
onCheckedChange :: act -> Attribute act
onCheckedChange = onEvent . OnCheckedChange

-- | The 'selected' property of the the target element has changed. This
-- handler is supported for <option> elements.
onSelectedChange :: act -> Attribute act
onSelectedChange = onEvent . OnSelectedChange

-- | The user has submitted the target form. This handler is supported for
-- <form> elements.
onSubmit :: act -> Attribute act
onSubmit = onEvent . OnSubmit

-- Mouse events
-------------------------------------------------------------------------------

-- | A simplified version of 'onClick' which watches for the 'LeftButton' only
-- and ignores the cursor position.
onClick :: act -> Attribute act
onClick = onEvent . OnClick

-- | A simplified version of 'onDoubleClick' which watches for the 'LeftButton'
-- only and ignores the cursor position.
onDoubleClick :: act -> Attribute act
onDoubleClick = onEvent . OnDoubleClick

-- | A simplified version of 'onMouseDown' which watches for the 'LeftButton'
-- only and ignores the cursor position.
onMouseDown :: act -> Attribute act
onMouseDown = onEvent . OnMouseDown

-- | A simplified version of 'onMouseUp' which watches for the 'LeftButton'
-- only and ignores the cursor position.
onMouseUp :: act -> Attribute act
onMouseUp = onEvent . OnMouseUp

-- | The mouse cursor has moved while positioned over the target element. The
-- mouse position at the time the event was fired is passed as a parameter to
-- the callback.
onMouseMove :: act -> Attribute act
onMouseMove = onEvent . OnMouseMove

-- | The mouse cursor has entered the region occupied by the target element.
-- The mouse position at the time the event was fired is passed as a parameter
-- to the callback.
onMouseEnter :: act -> Attribute act
onMouseEnter = onEvent . OnMouseEnter

-- | The mouse cursor has left the region occupied by the target element. The
-- mouse position at the time the event was fired is passed as a parameter to
-- the callback.
onMouseLeave :: act -> Attribute act
onMouseLeave = onEvent . OnMouseLeave

-- | Like MouseEnter, but handles bubbling differently.
onMouseOver :: act -> Attribute act
onMouseOver = onEvent . OnMouseOver

-- | Like MouseLeave, but handles bubbling differently.
onMouseOut :: act -> Attribute act
onMouseOut = onEvent . OnMouseOut

-- UI events
-------------------------------------------------------------------------------

-- | The the scroll-position of the page has changed. The amount by which it
-- has changed (in lines) is passed as a parameter to the callback.
onScroll :: act -> Attribute act
onScroll = onEvent . OnScroll

-- Wheel events
-------------------------------------------------------------------------------

-- | The user has moved the scroll-wheel. The amount by which the scroll
-- position of an infinitely large page is affected is passed as a parameter to
-- the callback.
onWheel :: act -> Attribute act
onWheel = onEvent . OnWheel


-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

-- | Register an event handler.
onEvent :: EventHandler act -> Attribute act
onEvent eh = Attribute (OnEvent eh)
{-# INLINE onEvent #-}
