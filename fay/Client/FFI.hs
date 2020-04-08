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

findProperty
  :: a
  -> Text
  -> Fay [Text]
findProperty = ffi "(function(){var arr = Array(); (function lookupProperty(object1, property){if (object1.hasOwnProperty(property)){arr.push(property); return true;} for (key in object1){if (object1[key] instanceof Object) {var hasProperty = lookupProperty(object1[key], property); if (hasProperty) { arr.push(key)}; return hasProperty;}}})(%1, %2); return arr.reverse();})()"

lookupProperty
  :: a
  -> [Text]
  -> Fay Text
lookupProperty = ffi "(function(ob, arr){var t = ob; for (var i = 0; i < arr.length; i++){t = t[arr[i]];}; return t; })(%1, %2)"

setProperty
  :: Automatic a
  -> [Text]
  -> Text
  -> Fay Text
setProperty = ffi "(function(ob, arr, newVal){var str = arr.join('.');var t = ob; var n = newVal.replace(/\"/g, '\\\\x22').replace(/'/g, '\\x27').replace(/\\n/, '\\\\x0A'); var cmd = 't.'+str+'=\"'+n+'\"'; eval(cmd); return JSON.stringify(t);})(%1, %2, %3)"

lookupProperty'
  :: a -- ^ Object to lookup.
  -> Text -- ^ Property to lookup.
  -> Fay Text -- ^ Property value if it is present. 'undefined' would be returned if no property is present in object tree.
lookupProperty' = ffi "(function lookupProperty(object1, property) {if (object1.hasOwnProperty(property)){return object1[property];} for (key in object1){if (object1[key] instanceof Object) {return lookupProperty(object1[key], property);}}})(%1, %2)"

setProperty'
  :: a -- ^ Object to change.
  -> Text -- ^ Property to lookup.
  -> Text -- ^ New value.
  -> a -- ^ Updated object.
setProperty' = ffi "(function(t){(function setProperty(object1, property, newValue) {if (object1.hasOwnProperty(property)){object1[property] = newValue;} for (key in object1) {if (object1[key] instanceof Object) {setProperty(object1[key], property, newValue);}}})(t, %2, %3); return t;})(%1)"
