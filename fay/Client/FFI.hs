{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.FFI where

import Data.Text (Text)
import DOM (Element)
import FFI

parse :: Text -> Fay (Automatic a)
parse = ffi "JSON.parse(%1)"

json :: Automatic a -> Fay Text
json = ffi "JSON.stringify(%1)"

attachToElemById :: Text -> Text -> Fay ()
attachToElemById = ffi "(function(eid,val){ document.getElementById(eid).innerHTML=val })(%1,%2)"

attachToParentById :: Text -> Text -> Fay ()
attachToParentById = ffi "(function(eid,val){ document.getElementById(eid).parentElement.innerHTML=val })(%1,%2)"

onClick :: Element -> Fay () -> Fay ()
onClick = ffi "%1.onclick=%2"

onKeyUp :: Element -> Fay () -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

onChange :: Element -> Fay () -> Fay ()
onChange = ffi "%1.onchange=%2"

log' :: a -> Fay ()
log' = ffi "console.log(%1)"
