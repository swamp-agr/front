{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Client.FFI where

import           Data.Text (Text)
import           DOM       (Element)
import           FFI

parse :: Text -> Fay (Automatic a)
parse = ffi "JSON.parse(%1)"

json :: Automatic a -> Fay Text
json = ffi "JSON.stringify(%1)"

attachToElemById :: Text -> Text -> Fay ()
attachToElemById = ffi "(function(eid,val){ document.getElementById(eid).innerHTML=val })(%1,%2)"

attachToParentById :: Text -> Text -> Fay ()
attachToParentById = ffi "(function(eid,val){ document.getElementById(eid).parentElement.innerHTML=val })(%1,%2)"

onClick :: Element -> (a -> Fay ()) -> Fay ()
onClick = ffi "%1.onclick=%2"

onKeyUp :: Element -> (a -> Fay ()) -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

onKeyDown :: Element -> (a -> Fay ()) -> Fay ()
onKeyDown = ffi "%1.onkeydown=%2"

onKeyPress :: Element -> (a -> Fay ()) -> Fay ()
onKeyPress = ffi "%1.onkeypress=%2"

onEnter :: Element -> (a -> Fay ()) -> Fay ()
onEnter = ffi "%1.onkeyup=%2"

keyCode :: a -> Fay Int
keyCode = ffi "%1.keyCode"

onChange :: Element -> (a -> Fay ()) -> Fay ()
onChange = ffi "%1.onchange=%2"

onBlur :: Element -> (a -> Fay ()) -> Fay ()
onBlur = ffi "%1.onblur=%2"

onDoubleClick :: Element -> (a -> Fay ()) -> Fay ()
onDoubleClick = ffi "%1.ondblclick=%2"

log' :: a -> Fay ()
log' = ffi "console.log(%1)"

value :: Element -> Fay Text
value = ffi "%1.value"
