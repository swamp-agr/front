
/*******************************************************************************
 * Misc.
 */


// Workaround for missing functionality in IE 8 and earlier.
if( Object.create === undefined ) {
  Object.create = function( o ) {
    function F(){}
    F.prototype = o;
    return new F();
  };
}

// Insert properties of b in place into a.
function Fay$$objConcat(a,b){
  for (var p in b) if (b.hasOwnProperty(p)){
    a[p] = b[p];
  }
  return a;
}

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    return function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    return function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.

        // Unserialize the JS values to Fay for the Fay callback.
        if (args == "automatic_function")
        {
          for (var i = 0; i < arguments.length; i++) {
            fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(["automatic"],arguments[i])),true);
          }
          return Fay$$fayToJs(["automatic"], fayFunc);
        }

        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    return Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Undefined
      ? undefined
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Null
      ? null
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    return Fay$$_(fayObj);
  }
  else if(base == "ptr")
    return fayObj;
  else if(base == "unknown")
    return Fay$$fayToJs(["automatic"], fayObj);
  else if(base == "automatic" && fayObj instanceof Function) {
    return Fay$$fayToJs(["function", "automatic_function"], fayObj);
  }
  else if(base == "automatic" || base == "user") {
    fayObj = Fay$$_(fayObj);

    if(fayObj instanceof Fay$$Cons || fayObj === null){
      // Serialize Fay list to JavaScript array.
      var arr = [];
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(["automatic"],fayObj.car));
        fayObj = Fay$$_(fayObj.cdr);
      }
      return arr;
    } else {
      var fayToJsFun = fayObj && fayObj.instance && Fay$$fayToJsHash[fayObj.instance];
      return fayToJsFun ? fayToJsFun(type,type[2],fayObj) : fayObj;
    }
  }

  throw new Error("Unhandled Fay->JS translation type: " + base);
}

// Stores the mappings from fay types to js objects.
// This will be populated by compiled modules.
var Fay$$fayToJsHash = {};

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += Fay$$_(fayObj.car);
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    return new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "function") {
    // Unserialize a function from JavaScript to a function that Fay can call.
    // So
    //
    //    var f = function(x,y,z){ … }
    //
    // becomes something like:
    //
    //    function(x){
    //      return function(y){
    //        return function(z){
    //          return new Fay$$$(function(){
    //            return Fay$$jsToFay(f(Fay$$fayTojs(x),
    //                                  Fay$$fayTojs(y),
    //                                  Fay$$fayTojs(z))
    //    }}}}};
    var returnType = args[args.length-1];
    var funArgs = args.slice(0,-1);

    if (jsObj.length > 0) {
      var makePartial = function(args){
        return function(arg){
          var i = args.length;
          var fayArg = Fay$$fayToJs(funArgs[i],arg);
          var newArgs = args.concat([fayArg]);
          if(newArgs.length == funArgs.length) {
            return new Fay$$$(function(){
              return Fay$$jsToFay(returnType,jsObj.apply(this,newArgs));
            });
          } else {
            return makePartial(newArgs);
          }
        };
      };
      return makePartial([]);
    }
    else
      return function (arg) {
        return Fay$$jsToFay(["automatic"], jsObj(Fay$$fayToJs(["automatic"], arg)));
      };
  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    // This is a special case, when String is explicit in the type signature,
    // with `Automatic' a string would not be decoded.
    return Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedList);
  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedTuple);
  }
  else if(base == "defined") {
    return jsObj === undefined
      ? new Fay.FFI._Undefined()
      : new Fay.FFI._Defined(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "nullable") {
    return jsObj === null
      ? new Fay.FFI._Null()
      : new Fay.FFI.Nullable(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
    return fayObj;
  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr") {
    return jsObj;
  }
  else if(base == "unknown")
    return Fay$$jsToFay(["automatic"], jsObj);
  else if(base == "automatic" && jsObj instanceof Function) {
    var type = [["automatic"]];
    for (var i = 0; i < jsObj.length; i++)
      type.push(["automatic"]);
    return Fay$$jsToFay(["function", type], jsObj);
  }
  else if(base == "automatic" && jsObj instanceof Array) {
    var list = null;
    for (var i = jsObj.length - 1; i >= 0; i--) {
      list = new Fay$$Cons(Fay$$jsToFay([base], jsObj[i]), list);
    }
    return list;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      var jsToFayFun = Fay$$jsToFayHash[jsObj["instance"]];
      return jsToFayFun ? jsToFayFun(type,type[2],jsObj) : jsObj;
    }
    else
      return jsObj;
  }

  throw new Error("Unhandled JS->Fay translation type: " + base);
}

// Stores the mappings from js objects to fay types.
// This will be populated by compiled modules.
var Fay$$jsToFayHash = {};

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.instance === lit2.instance) {
    for(var x in lit1) {
      if(!Fay$$equal(lit1[x],lit2[x]))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return Date.parse(str);
}

/*******************************************************************************
 * Data.Var
 */

function Fay$$Ref2(val){
  this.val = val;
}

function Fay$$Sig(){
  this.handlers = [];
}

function Fay$$Var(val){
  this.val = val;
  this.handlers = [];
}

// Helper used by Fay$$setValue and for merging
function Fay$$broadcastInternal(self, val, force){
  var handlers = self.handlers;
  var exceptions = [];
  for(var len = handlers.length, i = 0; i < len; i++) {
    try {
      force(handlers[i][1](val), true);
    } catch (e) {
      exceptions.push(e);
    }
  }
  // Rethrow the encountered exceptions.
  if (exceptions.length > 0) {
    console.error("Encountered " + exceptions.length + " exception(s) while broadcasing a change to ", self);
    for(var len = exceptions.length, i = 0; i < len; i++) {
      (function(exception) {
        setTimeout(function() { throw exception; }, 0);
      })(exceptions[i]);
    }
  }
}

function Fay$$setValue(self, val, force){
  if (self instanceof Fay$$Ref2) {
    self.val = val;
  } else if (self instanceof Fay$$Var) {
    self.val = val;
    Fay$$broadcastInternal(self, val, force);
  } else if (self instanceof Fay$$Sig) {
    Fay$$broadcastInternal(self, val, force);
  } else {
    throw "Fay$$setValue given something that's not a Ref2, Var, or Sig"
  }
}

function Fay$$subscribe(self, f){
  var key = {};
  self.handlers.push([key,f]);
  var searchStart = self.handlers.length - 1;
  return function(_){
    for(var i = Math.min(searchStart, self.handlers.length - 1); i >= 0; i--) {
      if(self.handlers[i][0] == key) {
        self.handlers = self.handlers.slice(0,i).concat(self.handlers.slice(i+1));
        return;
      }
    }
    return _; // This variable has to be used, otherwise Closure
              // strips it out and Fay serialization breaks.
  };
}

/*******************************************************************************
 * Application code.
 */

var Data = {};Data.Data = {};var Fay = {};Fay.FFI = {};Fay.FFI._Nullable = function Nullable(slot1){this.slot1 = slot1;};Fay.FFI._Nullable.prototype.instance = "Nullable";Fay.FFI.Nullable = function(slot1){return new Fay$$$(function(){return new Fay.FFI._Nullable(slot1);});};Fay.FFI._Null = function Null(){};Fay.FFI._Null.prototype.instance = "Null";Fay.FFI.Null = new Fay$$$(function(){return new Fay.FFI._Null();});Fay.FFI._Defined = function Defined(slot1){this.slot1 = slot1;};Fay.FFI._Defined.prototype.instance = "Defined";Fay.FFI.Defined = function(slot1){return new Fay$$$(function(){return new Fay.FFI._Defined(slot1);});};Fay.FFI._Undefined = function Undefined(){};Fay.FFI._Undefined.prototype.instance = "Undefined";Fay.FFI.Undefined = new Fay$$$(function(){return new Fay.FFI._Undefined();});Fay$$objConcat(Fay$$fayToJsHash,{"Nullable": function(type,argTypes,_obj){var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Null": function(type,argTypes,_obj){var obj_ = {"instance": "Null"};return obj_;},"Defined": function(type,argTypes,_obj){var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Undefined": function(type,argTypes,_obj){var obj_ = {"instance": "Undefined"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Nullable": function(type,argTypes,obj){return new Fay.FFI._Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Null": function(type,argTypes,obj){return new Fay.FFI._Null();},"Defined": function(type,argTypes,obj){return new Fay.FFI._Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Undefined": function(type,argTypes,obj){return new Fay.FFI._Undefined();}});var Prelude = {};Prelude._Just = function Just(slot1){this.slot1 = slot1;};Prelude._Just.prototype.instance = "Just";Prelude.Just = function(slot1){return new Fay$$$(function(){return new Prelude._Just(slot1);});};Prelude._Nothing = function Nothing(){};Prelude._Nothing.prototype.instance = "Nothing";Prelude.Nothing = new Fay$$$(function(){return new Prelude._Nothing();});Prelude._Left = function Left(slot1){this.slot1 = slot1;};Prelude._Left.prototype.instance = "Left";Prelude.Left = function(slot1){return new Fay$$$(function(){return new Prelude._Left(slot1);});};Prelude._Right = function Right(slot1){this.slot1 = slot1;};Prelude._Right.prototype.instance = "Right";Prelude.Right = function(slot1){return new Fay$$$(function(){return new Prelude._Right(slot1);});};Prelude.maybe = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof Prelude._Nothing) {var m = $p1;return m;}if (Fay$$_($p3) instanceof Prelude._Just) {var x = Fay$$_($p3).slot1;var f = $p2;return Fay$$_(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};Prelude.$62$$62$$61$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$_(Fay$$bind($p1)($p2));});};};Prelude.$62$$62$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$_(Fay$$then($p1)($p2));});};};Prelude.$_return = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$$_return(Fay$$fayToJs(["unknown"],$p1))));});};Prelude.fail = new Fay$$$(function(){return Prelude.error;});Prelude.when = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? m : Fay$$_(Fay$$$_return)(Fay$$unit);});};};Prelude.unless = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? Fay$$_(Fay$$$_return)(Fay$$unit) : m;});};};Prelude.forM = function($p1){return function($p2){return new Fay$$$(function(){var fn = $p2;var lst = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(fn))(lst));});};};Prelude.forM_ = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.forM_)(xs))(m));}if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};Prelude.mapM = function($p1){return function($p2){return new Fay$$$(function(){var lst = $p2;var fn = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(fn))(lst));});};};Prelude.mapM_ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.mapM_)(m))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};Prelude.$61$$60$$60$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(x))(f);});};};Prelude.$_void = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Fay$$then)(f))(Fay$$_(Fay$$$_return)(Fay$$unit));});};Prelude.$62$$61$$62$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(g);});};};};Prelude.$60$$61$$60$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var g = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(g);});};};};Prelude.sequence = function($p1){return new Fay$$$(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new Fay$$$(function(){var m$39$ = $p2;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){var x = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));});});});};};return Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);})();});};Prelude.sequence_ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude.sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};Prelude._GT = function GT(){};Prelude._GT.prototype.instance = "GT";Prelude.GT = new Fay$$$(function(){return new Prelude._GT();});Prelude._LT = function LT(){};Prelude._LT.prototype.instance = "LT";Prelude.LT = new Fay$$$(function(){return new Prelude._LT();});Prelude._EQ = function EQ(){};Prelude._EQ.prototype.instance = "EQ";Prelude.EQ = new Fay$$$(function(){return new Prelude._EQ();});Prelude.compare = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude.GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude.LT : Prelude.EQ;});};};Prelude.succ = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$add)(x))(1);});};Prelude.pred = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$sub)(x))(1);});};Prelude.enumFrom = function($p1){return new Fay$$$(function(){var i = $p1;return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude.enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));});};Prelude.enumFromTo = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;var i = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude.enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));});};};Prelude.enumFromBy = function($p1){return function($p2){return new Fay$$$(function(){var by = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude.enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));});};};Prelude.enumFromThen = function($p1){return function($p2){return new Fay$$$(function(){var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Prelude.enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));});};};Prelude.enumFromByTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};var pos = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);})();});};};};Prelude.enumFromThenTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);});};};};Prelude.fromIntegral = function($p1){return new Fay$$$(function(){return $p1;});};Prelude.fromInteger = function($p1){return new Fay$$$(function(){return $p1;});};Prelude.not = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(p) ? false : true;});};Prelude.otherwise = true;Prelude.show = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));});};Prelude.error = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());});};Prelude.$_undefined = new Fay$$$(function(){return Fay$$_(Prelude.error)(Fay$$list("Prelude.undefined"));});Prelude.either = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof Prelude._Left) {var a = Fay$$_($p3).slot1;var f = $p1;return Fay$$_(f)(a);}if (Fay$$_($p3) instanceof Prelude._Right) {var b = Fay$$_($p3).slot1;var g = $p2;return Fay$$_(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};Prelude.until = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude.until)(p))(f))(Fay$$_(f)(x));});};};};Prelude.$36$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));});};};Prelude.$_const = function($p1){return function($p2){return new Fay$$$(function(){var a = $p1;return a;});};};Prelude.id = function($p1){return new Fay$$$(function(){var x = $p1;return x;});};Prelude.$46$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(f)(Fay$$_(g)(x));});};};};Prelude.$36$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(f)(x);});};};Prelude.flip = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(y))(x);});};};};Prelude.curry = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(f)(Fay$$list([x,y]));});};};};Prelude.uncurry = function($p1){return function($p2){return new Fay$$$(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var x = Fay$$index(0,Fay$$_($tmp1));var y = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};Prelude.snd = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(1,Fay$$_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};Prelude.fst = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(0,Fay$$_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};Prelude.div = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.quot)(x))(y);});};};Prelude.mod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.rem)(x))(y);});};};Prelude.divMod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.quotRem)(x))(y);});};};Prelude.min = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.max = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.recip = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(1))(x);});};Prelude.negate = function($p1){return new Fay$$$(function(){var x = $p1;return (-(Fay$$_(x)));});};Prelude.abs = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.negate)(x) : x;});};Prelude.signum = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};Prelude.pi = new Fay$$$(function(){return Fay$$jsToFay_double(Math.PI);});Prelude.exp = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));});};Prelude.sqrt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));});};Prelude.log = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));});};Prelude.$42$$42$ = new Fay$$$(function(){return Prelude.unsafePow;});Prelude.$94$$94$ = new Fay$$$(function(){return Prelude.unsafePow;});Prelude.unsafePow = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.$94$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {return Fay$$_(Prelude.error)(Fay$$list("(^): negative exponent"));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {return 1;} else {if (Fay$$_(Fay$$_(Prelude.even)(b))) {return (function(){return new Fay$$$(function(){var x = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Prelude.quot)(b))(2));});return Fay$$_(Fay$$_(Fay$$mult)(x))(x);});})();}}}var b = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));});};};Prelude.logBase = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var b = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(x)))(Fay$$_(Prelude.log)(b));});};};Prelude.sin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));});};Prelude.tan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));});};Prelude.cos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));});};Prelude.asin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));});};Prelude.atan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));});};Prelude.acos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));});};Prelude.sinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);});};Prelude.tanh = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){return new Fay$$$(function(){var a = new Fay$$$(function(){return Fay$$_(Prelude.exp)(x);});var b = new Fay$$$(function(){return Fay$$_(Prelude.exp)((-(Fay$$_(x))));});return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));});})();});};Prelude.cosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);});};Prelude.asinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));});};Prelude.atanh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);});};Prelude.acosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));});};Prelude.properFraction = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){return new Fay$$$(function(){var a = new Fay$$$(function(){return Fay$$_(Prelude.truncate)(x);});return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude.fromIntegral)(a))]);});})();});};Prelude.truncate = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.ceiling)(x) : Fay$$_(Prelude.floor)(x);});};Prelude.round = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));});};Prelude.ceiling = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));});};Prelude.floor = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));});};Prelude.subtract = new Fay$$$(function(){return Fay$$_(Prelude.flip)(Fay$$sub);});Prelude.even = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude.rem)(x))(2)))(0);});};Prelude.odd = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.not)(Fay$$_(Prelude.even)(x));});};Prelude.gcd = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude.rem)(x))(y));});};};return Fay$$_(Fay$$_(go)(Fay$$_(Prelude.abs)(a)))(Fay$$_(Prelude.abs)(b));})();});};};Prelude.quot = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.quot$39$)(x))(y);});};};Prelude.quot$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));});};};Prelude.quotRem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$list([Fay$$_(Fay$$_(Prelude.quot)(x))(y),Fay$$_(Fay$$_(Prelude.rem)(x))(y)]);});};};Prelude.rem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.rem$39$)(x))(y);});};};Prelude.rem$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));});};};Prelude.lcm = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {return 0;}if (Fay$$_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return Fay$$_(Prelude.abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude.quot)(a))(Fay$$_(Fay$$_(Prelude.gcd)(a))(b))))(b));});};};Prelude.find = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude.Just)(x) : Fay$$_(Fay$$_(Prelude.find)(p))(xs);}if (Fay$$_($p2) === null) {return Prelude.Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};Prelude.filter = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude.filter)(p))(xs);}if (Fay$$_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};Prelude.$_null = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}return false;});};Prelude.map = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude.map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};Prelude.nub = function($p1){return new Fay$$$(function(){var ls = $p1;return Fay$$_(Fay$$_(Prelude.nub$39$)(ls))(null);});};Prelude.nub$39$ = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var ls = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude.elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};Prelude.elem = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));}if (Fay$$_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};Prelude.notElem = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var x = $p1;return Fay$$_(Prelude.not)(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));});};};Prelude.sort = new Fay$$$(function(){return Fay$$_(Prelude.sortBy)(Prelude.compare);});Prelude.sortBy = function($p1){return new Fay$$$(function(){var cmp = $p1;return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Prelude.insertBy)(cmp)))(null);});};Prelude.insertBy = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (Fay$$_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (Fay$$_($tmp2) instanceof Prelude._GT) {return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude.insertBy)(cmp))(x))(ys$39$));}return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);})(Fay$$_(Fay$$_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};Prelude.conc = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.conc)(xs))(ys));}var ys = $p2;if (Fay$$_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};Prelude.concat = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.foldr)(Prelude.conc))(null);});Prelude.concatMap = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.$43$$43$))(f)))(null);});};Prelude.foldr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};Prelude.foldr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return x;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude.foldr1)(f))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Prelude.error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};Prelude.foldl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};Prelude.foldl1 = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(x))(xs);}if (Fay$$_($p2) === null) {return Fay$$_(Prelude.error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};Prelude.$43$$43$ = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.conc)(x))(y);});};};Prelude.$33$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("(!!): index too large"));}if (Fay$$_($p2) === 0) {var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude.error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);})();});};};Prelude.head = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("head: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};Prelude.tail = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("tail: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};Prelude.init = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("init: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {return null;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude.init)(t));}throw ["unhandled case in init",[$p1]];});};Prelude.last = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("last: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return a;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Prelude.last)(t);}throw ["unhandled case in last",[$p1]];});};Prelude.iterate = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.iterate)(f))(Fay$$_(f)(x)));});};};Prelude.repeat = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude.repeat)(x));});};Prelude.replicate = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}var x = $p2;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));});};};Prelude.cycle = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};Prelude.take = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};Prelude.drop = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return xs;}if (Fay$$_($p2) === null) {return null;}var xss = $p2;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? xss : Fay$$_(Fay$$_(Prelude.drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};Prelude.splitAt = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return Fay$$list([null,xs]);}if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]) : (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};Prelude.takeWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};Prelude.dropWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude.dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};Prelude.span = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};Prelude.$_break = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(Prelude.span)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.not))(p));});};Prelude.zipWith = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith)(f))(as))(bs));}}return null;});};};};Prelude.zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};Prelude.zip = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude.zip)(as))(bs));}}return null;});};};Prelude.zip3 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude.zip3)(as))(bs))(cs));}}}return null;});};};};Prelude.unzip = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude.unzip)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};Prelude.unzip3 = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),3)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var z = Fay$$index(2,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),3)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));var zs = Fay$$index(2,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude.unzip3)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};Prelude.lines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));});};return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {return Fay$$list([a]);}}if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.$_break)(isLineBreak))(s));})();});};Prelude.unlines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var l = $tmp1.car;var ls = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.$43$$43$)(l))(Fay$$_(Fay$$_(Fay$$cons)("\n"))(Fay$$_(Prelude.unlines)(ls)));}throw ["unhandled case in unlines",[$p1]];});};Prelude.words = function($p1){return new Fay$$$(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.$_break)(isSpace))(s));});};var isSpace = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Prelude.elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude.dropWhile)(isSpace))(str));})();});};Prelude.unwords = new Fay$$$(function(){return Fay$$_(Prelude.intercalate)(Fay$$list(" "));});Prelude.and = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude.and)(xs));}throw ["unhandled case in and",[$p1]];});};Prelude.or = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return false;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude.or)(xs));}throw ["unhandled case in or",[$p1]];});};Prelude.any = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return false;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.any)(p))(xs));}throw ["unhandled case in any",[$p1,$p2]];});};};Prelude.all = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return true;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.all)(p))(xs));}throw ["unhandled case in all",[$p1,$p2]];});};};Prelude.intersperse = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};Prelude.prependToAll = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};Prelude.intercalate = function($p1){return function($p2){return new Fay$$$(function(){var xss = $p2;var xs = $p1;return Fay$$_(Prelude.concat)(Fay$$_(Fay$$_(Prelude.intersperse)(xs))(xss));});};};Prelude.maximum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("maximum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.max))(xs);});};Prelude.minimum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("minimum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.min))(xs);});};Prelude.product = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$mult))(1))(xs);});};Prelude.sum = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$add))(0))(xs);});};Prelude.scanl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var l = $p3;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};Prelude.scanl1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};Prelude.scanr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude.$_undefined;})(Fay$$_(Fay$$_(Fay$$_(Prelude.scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};Prelude.scanr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return Fay$$list([x]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude.$_undefined;})(Fay$$_(Fay$$_(Prelude.scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};Prelude.lookup = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var _key = $p1;return Prelude.Nothing;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude.Just)(y) : Fay$$_(Fay$$_(Prelude.lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};Prelude.length = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Prelude.length$39$)(0))(xs);});};Prelude.length$39$ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return Fay$$_(Fay$$_(Prelude.length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);}var acc = $p1;return acc;});};};Prelude.reverse = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(Prelude.reverse)(xs)))(Fay$$list([x]));}if (Fay$$_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};Prelude.print = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));});};Prelude.putStrLn = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));});};Prelude.ifThenElse = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var b = $p3;var a = $p2;var p = $p1;return Fay$$_(p) ? a : b;});};};};Fay$$objConcat(Fay$$fayToJsHash,{"Just": function(type,argTypes,_obj){var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Nothing": function(type,argTypes,_obj){var obj_ = {"instance": "Nothing"};return obj_;},"Left": function(type,argTypes,_obj){var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Right": function(type,argTypes,_obj){var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"GT": function(type,argTypes,_obj){var obj_ = {"instance": "GT"};return obj_;},"LT": function(type,argTypes,_obj){var obj_ = {"instance": "LT"};return obj_;},"EQ": function(type,argTypes,_obj){var obj_ = {"instance": "EQ"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Just": function(type,argTypes,obj){return new Prelude._Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Nothing": function(type,argTypes,obj){return new Prelude._Nothing();},"Left": function(type,argTypes,obj){return new Prelude._Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Right": function(type,argTypes,obj){return new Prelude._Right(Fay$$jsToFay(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"GT": function(type,argTypes,obj){return new Prelude._GT();},"LT": function(type,argTypes,obj){return new Prelude._LT();},"EQ": function(type,argTypes,obj){return new Prelude._EQ();}});var FFI = {};Data.Nullable = {};Data.Nullable.fromNullable = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Fay.FFI._Nullable) {var x = Fay$$_($p1).slot1;return Fay$$_(Prelude.Just)(x);}if (Fay$$_($p1) instanceof Fay.FFI._Null) {return Prelude.Nothing;}throw ["unhandled case in fromNullable",[$p1]];});};Data.Nullable.toNullable = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Prelude._Just) {var x = Fay$$_($p1).slot1;return Fay$$_(Fay.FFI.Nullable)(x);}if (Fay$$_($p1) instanceof Prelude._Nothing) {return Fay.FFI.Null;}throw ["unhandled case in toNullable",[$p1]];});};Data.Text = {};Data.Text.intercalate = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p2).join(Fay$$fayToJs(["user","Text",[]],$p1)));});};};Data.Text.fromString = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_string($p1));});};Data.Text.snoc = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Char",[]],$p2));});};};Data.Text.cons = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Char",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));});};};Data.Text.pack = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_string($p1));});};Data.Text.unpack = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string(Fay$$fayToJs(["user","Text",[]],$p1));});};Data.Text.append = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));});};};Data.Text.$60$$62$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));});};};Data.Text.length = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Fay$$fayToJs(["user","Text",[]],$p1).length);});};Data.Text.$_null = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Text",[]],$p1).length == 0);});};Data.Text.take = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).substring(0,Fay$$fayToJs_int($p1)));});};};Data.Text.drop = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).substring(Fay$$fayToJs_int($p1)));});};};Data.Text.empty = new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],"");});Data.Text.lines = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["list",[["user","Text",[]]]],Fay$$fayToJs(["user","Text",[]],$p1).split('\n'));});};Data.Text.unlines = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p1).join('\n'));});};Data.Text.isPrefixOf = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Text",[]],$p2).lastIndexOf(Fay$$fayToJs(["user","Text",[]],$p1), 0) == 0);});};};Data.Text.intersperse = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).split('').join(Fay$$fayToJs(["user","Char",[]],$p1)));});};};Data.Text.reverse = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).split('').reverse().join(''));});};Data.Text.stripSuffix = function($p1){return function($p2){return new Fay$$$(function(){var text = $p2;var prefix = $p1;return (function(){var extract = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["nullable",[["user","Text",[]]]],(function(suffix,text){ return text.substring(text.length - suffix.length) == suffix? text.substring(0,text.length - suffix.length) : null; })(Fay$$fayToJs(["user","Text",[]],$p1),Fay$$fayToJs(["user","Text",[]],$p2)));});};};return Fay$$_(Data.Nullable.fromNullable)(Fay$$_(Fay$$_(extract)(prefix))(text));})();});};};Data.Text.splitOn = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["list",[["user","Text",[]]]],Fay$$fayToJs(["user","Text",[]],$p2).split(Fay$$fayToJs(["user","Text",[]],$p1)));});};};Data.Text.putStrLn = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],console.log('%s',Fay$$fayToJs(["user","Text",[]],$p1))));});};Data.Text.toShortest = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_double($p1).toString());});};Data.Text.showInt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_int($p1).toString());});};Data.Text.uncons = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Maybe",[["tuple",[["user","Char",[]],["user","Text",[]]]]]],Fay$$fayToJs(["user","Text",[]],$p1)[0] ? { instance: 'Just', slot1 : [Fay$$fayToJs(["user","Text",[]],$p1)[0],Fay$$fayToJs(["user","Text",[]],$p1).slice(1)] } : { instance : 'Nothing' });});};Data.Text.head = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Char",[]],Fay$$fayToJs(["user","Text",[]],$p1)[0] || (function () {throw new Error('Data.Text.head: empty Text'); }()));});};Data.Text.last = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Char",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1)[Fay$$fayToJs(["user","Text",[]],$p1).length-1] : (function() { throw new Error('Data.Text.last: empty Text') })());});};Data.Text.tail = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1).slice(1) : (function () { throw new Error('Data.Text.tail: empty Text') })());});};Data.Text.init = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1).slice(0,-1) : (function () { throw new Error('Data.Text.init: empty Text') })());});};Data.Text.map = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],[].map.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["user","Char",[]]]],$p1)).join(''));});};};Data.Text.toLower = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toLowerCase());});};Data.Text.toUpper = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toUpperCase());});};Data.Text.concat = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p1).join(''));});};Data.Text.concatMap = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["user","Text",[]],[].map.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["user","Text",[]]]],$p1)).join(''));});};};Data.Text.any = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_bool([].filter.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1)).length > 0);});};};Data.Text.all = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_bool([].filter.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1)).length == Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1).length);});};};Data.Text.maximum = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Char",[]],(function (s) {    if (s === '') { throw new Error('Data.Text.maximum: empty string'); }    var max = s[0];    for (var i = 1; i < s.length; s++) {      if (s[i] > max) { max = s[i]; }    }    return max;  })(Fay$$fayToJs(["user","Text",[]],$p1)));});};Data.Text.minimum = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Char",[]],(function (s) {    if (s === '') { throw new Error('Data.Text.maximum: empty string'); }    var min = s[0];    for (var i = 1; i < s.length; s++) {      if (s[i] < min) { min = s[i]; }    }    return min;  })(Fay$$fayToJs(["user","Text",[]],$p1)));});};var WebSockets = {};WebSockets._Connecting = function Connecting(){};WebSockets._Connecting.prototype.instance = "Connecting";WebSockets.Connecting = new Fay$$$(function(){return new WebSockets._Connecting();});WebSockets._Open = function Open(){};WebSockets._Open.prototype.instance = "Open";WebSockets.Open = new Fay$$$(function(){return new WebSockets._Open();});WebSockets._Closing = function Closing(){};WebSockets._Closing.prototype.instance = "Closing";WebSockets.Closing = new Fay$$$(function(){return new WebSockets._Closing();});WebSockets._Closed = function Closed(){};WebSockets._Closed.prototype.instance = "Closed";WebSockets.Closed = new Fay$$$(function(){return new WebSockets._Closed();});WebSockets.getWsUrl = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],window['location']['href'].replace('http:', 'ws:').replace('https:', 'wss:')));});WebSockets.onOpen = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],console.log('WebSocket is up and running')));});};WebSockets.onClose = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","WebSocket",[]],$p1).onclose=Fay$$fayToJs(["action",[["unknown"]]],$p2)));});};};WebSockets.onError = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","WebSocket",[]],$p1).onerror=Fay$$fayToJs(["action",[["unknown"]]],$p2)));});};};WebSockets.eventData = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","WSEvent",[]],$p1)['data']));});};WebSockets.onMessage = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(e){console.log(e); console.log(e.data);})(Fay$$fayToJs(["user","WSEvent",[]],$p1))));});};WebSockets.websocket = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","WebSocket",[]],(function (){var conn = new WebSocket(Fay$$fayToJs(["user","Text",[]],$p1)); conn.onopen = Fay$$fayToJs(["function",[["user","WSEvent",[]],["action",[["unknown"]]]]],$p2), conn.onmessage = Fay$$fayToJs(["function",[["user","WSEvent",[]],["action",[["unknown"]]]]],$p3), conn.onerror = Fay$$fayToJs(["function",[["user","WSEvent",[]],["action",[["unknown"]]]]],$p4), conn.onclose = Fay$$fayToJs(["function",[["user","WSEvent",[]],["action",[["unknown"]]]]],$p5);return conn})()));});};};};};};WebSockets.sendWS = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","WebSocket",[]],$p1).send(Fay$$fayToJs(["user","Text",[]],$p2))));});};};WebSockets.close = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","WebSocket",[]],$p1).close));});};WebSockets.target = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","WebSocket",[]],Fay$$fayToJs(["user","WSEvent",[]],$p1)['target']));});};Fay$$objConcat(Fay$$fayToJsHash,{"Connecting": function(type,argTypes,_obj){var obj_ = {"instance": "Connecting"};return obj_;},"Open": function(type,argTypes,_obj){var obj_ = {"instance": "Open"};return obj_;},"Closing": function(type,argTypes,_obj){var obj_ = {"instance": "Closing"};return obj_;},"Closed": function(type,argTypes,_obj){var obj_ = {"instance": "Closed"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Connecting": function(type,argTypes,obj){return new WebSockets._Connecting();},"Open": function(type,argTypes,obj){return new WebSockets._Open();},"Closing": function(type,argTypes,obj){return new WebSockets._Closing();},"Closed": function(type,argTypes,obj){return new WebSockets._Closed();}});var Bridge = {};Bridge._OnKeyDown = function OnKeyDown(slot1){this.slot1 = slot1;};Bridge._OnKeyDown.prototype.instance = "OnKeyDown";Bridge.OnKeyDown = function(slot1){return new Fay$$$(function(){return new Bridge._OnKeyDown(slot1);});};Bridge._OnKeyUp = function OnKeyUp(slot1){this.slot1 = slot1;};Bridge._OnKeyUp.prototype.instance = "OnKeyUp";Bridge.OnKeyUp = function(slot1){return new Fay$$$(function(){return new Bridge._OnKeyUp(slot1);});};Bridge._OnKeyPress = function OnKeyPress(slot1){this.slot1 = slot1;};Bridge._OnKeyPress.prototype.instance = "OnKeyPress";Bridge.OnKeyPress = function(slot1){return new Fay$$$(function(){return new Bridge._OnKeyPress(slot1);});};Bridge._OnFocus = function OnFocus(slot1){this.slot1 = slot1;};Bridge._OnFocus.prototype.instance = "OnFocus";Bridge.OnFocus = function(slot1){return new Fay$$$(function(){return new Bridge._OnFocus(slot1);});};Bridge._OnBlur = function OnBlur(slot1){this.slot1 = slot1;};Bridge._OnBlur.prototype.instance = "OnBlur";Bridge.OnBlur = function(slot1){return new Fay$$$(function(){return new Bridge._OnBlur(slot1);});};Bridge._OnValueChange = function OnValueChange(slot1){this.slot1 = slot1;};Bridge._OnValueChange.prototype.instance = "OnValueChange";Bridge.OnValueChange = function(slot1){return new Fay$$$(function(){return new Bridge._OnValueChange(slot1);});};Bridge._OnCheckedChange = function OnCheckedChange(slot1){this.slot1 = slot1;};Bridge._OnCheckedChange.prototype.instance = "OnCheckedChange";Bridge.OnCheckedChange = function(slot1){return new Fay$$$(function(){return new Bridge._OnCheckedChange(slot1);});};Bridge._OnSelectedChange = function OnSelectedChange(slot1){this.slot1 = slot1;};Bridge._OnSelectedChange.prototype.instance = "OnSelectedChange";Bridge.OnSelectedChange = function(slot1){return new Fay$$$(function(){return new Bridge._OnSelectedChange(slot1);});};Bridge._OnSubmit = function OnSubmit(slot1){this.slot1 = slot1;};Bridge._OnSubmit.prototype.instance = "OnSubmit";Bridge.OnSubmit = function(slot1){return new Fay$$$(function(){return new Bridge._OnSubmit(slot1);});};Bridge._OnClick = function OnClick(slot1){this.slot1 = slot1;};Bridge._OnClick.prototype.instance = "OnClick";Bridge.OnClick = function(slot1){return new Fay$$$(function(){return new Bridge._OnClick(slot1);});};Bridge._OnDoubleClick = function OnDoubleClick(slot1){this.slot1 = slot1;};Bridge._OnDoubleClick.prototype.instance = "OnDoubleClick";Bridge.OnDoubleClick = function(slot1){return new Fay$$$(function(){return new Bridge._OnDoubleClick(slot1);});};Bridge._OnMouseDown = function OnMouseDown(slot1){this.slot1 = slot1;};Bridge._OnMouseDown.prototype.instance = "OnMouseDown";Bridge.OnMouseDown = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseDown(slot1);});};Bridge._OnMouseUp = function OnMouseUp(slot1){this.slot1 = slot1;};Bridge._OnMouseUp.prototype.instance = "OnMouseUp";Bridge.OnMouseUp = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseUp(slot1);});};Bridge._OnMouseMove = function OnMouseMove(slot1){this.slot1 = slot1;};Bridge._OnMouseMove.prototype.instance = "OnMouseMove";Bridge.OnMouseMove = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseMove(slot1);});};Bridge._OnMouseEnter = function OnMouseEnter(slot1){this.slot1 = slot1;};Bridge._OnMouseEnter.prototype.instance = "OnMouseEnter";Bridge.OnMouseEnter = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseEnter(slot1);});};Bridge._OnMouseLeave = function OnMouseLeave(slot1){this.slot1 = slot1;};Bridge._OnMouseLeave.prototype.instance = "OnMouseLeave";Bridge.OnMouseLeave = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseLeave(slot1);});};Bridge._OnMouseOver = function OnMouseOver(slot1){this.slot1 = slot1;};Bridge._OnMouseOver.prototype.instance = "OnMouseOver";Bridge.OnMouseOver = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseOver(slot1);});};Bridge._OnMouseOut = function OnMouseOut(slot1){this.slot1 = slot1;};Bridge._OnMouseOut.prototype.instance = "OnMouseOut";Bridge.OnMouseOut = function(slot1){return new Fay$$$(function(){return new Bridge._OnMouseOut(slot1);});};Bridge._OnScroll = function OnScroll(slot1){this.slot1 = slot1;};Bridge._OnScroll.prototype.instance = "OnScroll";Bridge.OnScroll = function(slot1){return new Fay$$$(function(){return new Bridge._OnScroll(slot1);});};Bridge._OnWheel = function OnWheel(slot1){this.slot1 = slot1;};Bridge._OnWheel.prototype.instance = "OnWheel";Bridge.OnWheel = function(slot1){return new Fay$$$(function(){return new Bridge._OnWheel(slot1);});};Bridge._PingPong = function PingPong(){};Bridge._PingPong.prototype.instance = "PingPong";Bridge.PingPong = new Fay$$$(function(){return new Bridge._PingPong();});Bridge._Send = function Send(slot1){this.slot1 = slot1;};Bridge._Send.prototype.instance = "Send";Bridge.Send = function(slot1){return new Fay$$$(function(){return new Bridge._Send(slot1);});};Bridge._AskEvents = function AskEvents(){};Bridge._AskEvents.prototype.instance = "AskEvents";Bridge.AskEvents = new Fay$$$(function(){return new Bridge._AskEvents();});Bridge._EmptyCmd = function EmptyCmd(){};Bridge._EmptyCmd.prototype.instance = "EmptyCmd";Bridge.EmptyCmd = new Fay$$$(function(){return new Bridge._EmptyCmd();});Bridge._ExecuteClient = function ExecuteClient(slot1,slot2,slot3){this.slot1 = slot1;this.slot2 = slot2;this.slot3 = slot3;};Bridge._ExecuteClient.prototype.instance = "ExecuteClient";Bridge.ExecuteClient = function(slot1){return function(slot2){return function(slot3){return new Fay$$$(function(){return new Bridge._ExecuteClient(slot1,slot2,slot3);});};};};Bridge._ExecuteAll = function ExecuteAll(){};Bridge._ExecuteAll.prototype.instance = "ExecuteAll";Bridge.ExecuteAll = new Fay$$$(function(){return new Bridge._ExecuteAll();});Bridge._ExecuteExcept = function ExecuteExcept(){};Bridge._ExecuteExcept.prototype.instance = "ExecuteExcept";Bridge.ExecuteExcept = new Fay$$$(function(){return new Bridge._ExecuteExcept();});Bridge._ClientTask = function ClientTask(executeRenderHtml,executeAction){this.executeRenderHtml = executeRenderHtml;this.executeAction = executeAction;};Bridge._ClientTask.prototype.instance = "ClientTask";Bridge.ClientTask = function(executeRenderHtml){return function(executeAction){return new Fay$$$(function(){return new Bridge._ClientTask(executeRenderHtml,executeAction);});};};Bridge.executeRenderHtml = function(x){return new Fay$$$(function(){return Fay$$_(x).executeRenderHtml;});};Bridge.executeAction = function(x){return new Fay$$$(function(){return Fay$$_(x).executeAction;});};Bridge._AttachText = function AttachText(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};Bridge._AttachText.prototype.instance = "AttachText";Bridge.AttachText = function(slot1){return function(slot2){return new Fay$$$(function(){return new Bridge._AttachText(slot1,slot2);});};};Bridge._AttachDOM = function AttachDOM(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};Bridge._AttachDOM.prototype.instance = "AttachDOM";Bridge.AttachDOM = function(slot1){return function(slot2){return new Fay$$$(function(){return new Bridge._AttachDOM(slot1,slot2);});};};Bridge._CallbackAction = function CallbackAction(slot1){this.slot1 = slot1;};Bridge._CallbackAction.prototype.instance = "CallbackAction";Bridge.CallbackAction = function(slot1){return new Fay$$$(function(){return new Bridge._CallbackAction(slot1);});};Bridge._Action = function Action(slot1,slot2,slot3){this.slot1 = slot1;this.slot2 = slot2;this.slot3 = slot3;};Bridge._Action.prototype.instance = "Action";Bridge.Action = function(slot1){return function(slot2){return function(slot3){return new Fay$$$(function(){return new Bridge._Action(slot1,slot2,slot3);});};};};Bridge._RecordAction = function RecordAction(){};Bridge._RecordAction.prototype.instance = "RecordAction";Bridge.RecordAction = new Fay$$$(function(){return new Bridge._RecordAction();});Bridge._ObjectAction = function ObjectAction(){};Bridge._ObjectAction.prototype.instance = "ObjectAction";Bridge.ObjectAction = new Fay$$$(function(){return new Bridge._ObjectAction();});Bridge.ppIncomingCommand = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof Bridge._AskEvents) {return Fay$$list("AskEvents");}if (Fay$$_($p1) instanceof Bridge._Send) {return Fay$$list("SendObjectAction");}if (Fay$$_($p1) instanceof Bridge._PingPong) {return Fay$$list("PingPong");}throw ["unhandled case in ppIncomingCommand",[$p1]];});};Fay$$objConcat(Fay$$fayToJsHash,{"OnKeyDown": function(type,argTypes,_obj){var obj_ = {"instance": "OnKeyDown"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnKeyUp": function(type,argTypes,_obj){var obj_ = {"instance": "OnKeyUp"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnKeyPress": function(type,argTypes,_obj){var obj_ = {"instance": "OnKeyPress"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnFocus": function(type,argTypes,_obj){var obj_ = {"instance": "OnFocus"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnBlur": function(type,argTypes,_obj){var obj_ = {"instance": "OnBlur"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnValueChange": function(type,argTypes,_obj){var obj_ = {"instance": "OnValueChange"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnCheckedChange": function(type,argTypes,_obj){var obj_ = {"instance": "OnCheckedChange"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnSelectedChange": function(type,argTypes,_obj){var obj_ = {"instance": "OnSelectedChange"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnSubmit": function(type,argTypes,_obj){var obj_ = {"instance": "OnSubmit"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnClick": function(type,argTypes,_obj){var obj_ = {"instance": "OnClick"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnDoubleClick": function(type,argTypes,_obj){var obj_ = {"instance": "OnDoubleClick"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseDown": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseDown"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseUp": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseUp"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseMove": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseMove"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseEnter": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseEnter"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseLeave": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseLeave"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseOver": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseOver"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnMouseOut": function(type,argTypes,_obj){var obj_ = {"instance": "OnMouseOut"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnScroll": function(type,argTypes,_obj){var obj_ = {"instance": "OnScroll"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"OnWheel": function(type,argTypes,_obj){var obj_ = {"instance": "OnWheel"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"PingPong": function(type,argTypes,_obj){var obj_ = {"instance": "PingPong"};return obj_;},"Send": function(type,argTypes,_obj){var obj_ = {"instance": "Send"};var obj_slot1 = Fay$$fayToJs(["user","Action",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"AskEvents": function(type,argTypes,_obj){var obj_ = {"instance": "AskEvents"};return obj_;},"EmptyCmd": function(type,argTypes,_obj){var obj_ = {"instance": "EmptyCmd"};return obj_;},"ExecuteClient": function(type,argTypes,_obj){var obj_ = {"instance": "ExecuteClient"};var obj_slot1 = Fay$$fayToJs(["user","ClientId",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["user","ClientTask",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}var obj_slot3 = Fay$$fayToJs(["user","ExecuteStrategy",[]],_obj.slot3);if (undefined !== obj_slot3) {obj_['slot3'] = obj_slot3;}return obj_;},"ExecuteAll": function(type,argTypes,_obj){var obj_ = {"instance": "ExecuteAll"};return obj_;},"ExecuteExcept": function(type,argTypes,_obj){var obj_ = {"instance": "ExecuteExcept"};return obj_;},"ClientTask": function(type,argTypes,_obj){var obj_ = {"instance": "ClientTask"};var obj_executeRenderHtml = Fay$$fayToJs(["list",[["user","RenderHtml",[]]]],_obj.executeRenderHtml);if (undefined !== obj_executeRenderHtml) {obj_['executeRenderHtml'] = obj_executeRenderHtml;}var obj_executeAction = Fay$$fayToJs(["list",[["user","CallbackAction",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]]]],_obj.executeAction);if (undefined !== obj_executeAction) {obj_['executeAction'] = obj_executeAction;}return obj_;},"AttachText": function(type,argTypes,_obj){var obj_ = {"instance": "AttachText"};var obj_slot1 = Fay$$fayToJs(["user","ElementId",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["user","HtmlText",[]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;},"AttachDOM": function(type,argTypes,_obj){var obj_ = {"instance": "AttachDOM"};var obj_slot1 = Fay$$fayToJs(["user","ElementId",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["user","HtmlText",[]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;},"CallbackAction": function(type,argTypes,_obj){var obj_ = {"instance": "CallbackAction"};var obj_slot1 = Fay$$fayToJs(["user","EventHandler",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Action": function(type,argTypes,_obj){var obj_ = {"instance": "Action"};var obj_slot1 = Fay$$fayToJs(["user","ElementId",[]],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["user","ActionType",[]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}var obj_slot3 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot3);if (undefined !== obj_slot3) {obj_['slot3'] = obj_slot3;}return obj_;},"RecordAction": function(type,argTypes,_obj){var obj_ = {"instance": "RecordAction"};return obj_;},"ObjectAction": function(type,argTypes,_obj){var obj_ = {"instance": "ObjectAction"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"OnKeyDown": function(type,argTypes,obj){return new Bridge._OnKeyDown(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnKeyUp": function(type,argTypes,obj){return new Bridge._OnKeyUp(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnKeyPress": function(type,argTypes,obj){return new Bridge._OnKeyPress(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnFocus": function(type,argTypes,obj){return new Bridge._OnFocus(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnBlur": function(type,argTypes,obj){return new Bridge._OnBlur(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnValueChange": function(type,argTypes,obj){return new Bridge._OnValueChange(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnCheckedChange": function(type,argTypes,obj){return new Bridge._OnCheckedChange(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnSelectedChange": function(type,argTypes,obj){return new Bridge._OnSelectedChange(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnSubmit": function(type,argTypes,obj){return new Bridge._OnSubmit(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnClick": function(type,argTypes,obj){return new Bridge._OnClick(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnDoubleClick": function(type,argTypes,obj){return new Bridge._OnDoubleClick(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseDown": function(type,argTypes,obj){return new Bridge._OnMouseDown(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseUp": function(type,argTypes,obj){return new Bridge._OnMouseUp(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseMove": function(type,argTypes,obj){return new Bridge._OnMouseMove(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseEnter": function(type,argTypes,obj){return new Bridge._OnMouseEnter(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseLeave": function(type,argTypes,obj){return new Bridge._OnMouseLeave(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseOver": function(type,argTypes,obj){return new Bridge._OnMouseOver(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnMouseOut": function(type,argTypes,obj){return new Bridge._OnMouseOut(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnScroll": function(type,argTypes,obj){return new Bridge._OnScroll(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"OnWheel": function(type,argTypes,obj){return new Bridge._OnWheel(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"PingPong": function(type,argTypes,obj){return new Bridge._PingPong();},"Send": function(type,argTypes,obj){return new Bridge._Send(Fay$$jsToFay(["user","Action",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],obj["slot1"]));},"AskEvents": function(type,argTypes,obj){return new Bridge._AskEvents();},"EmptyCmd": function(type,argTypes,obj){return new Bridge._EmptyCmd();},"ExecuteClient": function(type,argTypes,obj){return new Bridge._ExecuteClient(Fay$$jsToFay(["user","ClientId",[]],obj["slot1"]),Fay$$jsToFay(["user","ClientTask",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],obj["slot2"]),Fay$$jsToFay(["user","ExecuteStrategy",[]],obj["slot3"]));},"ExecuteAll": function(type,argTypes,obj){return new Bridge._ExecuteAll();},"ExecuteExcept": function(type,argTypes,obj){return new Bridge._ExecuteExcept();},"ClientTask": function(type,argTypes,obj){return new Bridge._ClientTask(Fay$$jsToFay(["list",[["user","RenderHtml",[]]]],obj["executeRenderHtml"]),Fay$$jsToFay(["list",[["user","CallbackAction",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]]]],obj["executeAction"]));},"AttachText": function(type,argTypes,obj){return new Bridge._AttachText(Fay$$jsToFay(["user","ElementId",[]],obj["slot1"]),Fay$$jsToFay(["user","HtmlText",[]],obj["slot2"]));},"AttachDOM": function(type,argTypes,obj){return new Bridge._AttachDOM(Fay$$jsToFay(["user","ElementId",[]],obj["slot1"]),Fay$$jsToFay(["user","HtmlText",[]],obj["slot2"]));},"CallbackAction": function(type,argTypes,obj){return new Bridge._CallbackAction(Fay$$jsToFay(["user","EventHandler",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],obj["slot1"]));},"Action": function(type,argTypes,obj){return new Bridge._Action(Fay$$jsToFay(["user","ElementId",[]],obj["slot1"]),Fay$$jsToFay(["user","ActionType",[]],obj["slot2"]),Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot3"]));},"RecordAction": function(type,argTypes,obj){return new Bridge._RecordAction();},"ObjectAction": function(type,argTypes,obj){return new Bridge._ObjectAction();}});var DOM = {};DOM.getWindow = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Global",[]],window));});DOM.getDocument = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Document",[]],window.document));});DOM.getBody = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document.body));});DOM.getElementById = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['getElementById'](Fay$$fayToJs(["user","Text",[]],$p1))));});};DOM.getElementsByName = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Element",[]]]],document['getElementsByName'](Fay$$fayToJs(["user","Text",[]],$p1))));});};DOM.addEvent = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['addEventListener'](Fay$$fayToJs(["user","Text",[]],$p1),Fay$$fayToJs(["action",[["unknown"]]],$p2))));});};};DOM.removeEvent = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['removeEventListener'](Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p3))));});};};};DOM.stopProp = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Event",[]],$p1)['stopPropagation']()));});};DOM.preventDefault = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Event",[]],$p1)['preventDefault']()));});};DOM.createElement = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],window['document']['createElement'](Fay$$fayToJs(["user","Text",[]],$p1))));});};DOM.appendChild = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['appendChild'](Fay$$fayToJs(["user","Element",[]],$p2))));});};};DOM.appendChildBefore = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['parentNode']['insertBefore'](Fay$$fayToJs(["user","Element",[]],$p2), Fay$$fayToJs(["user","Element",[]],$p1))));});};};DOM.removeChild = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['removeChild'](Fay$$fayToJs(["user","Element",[]],$p2))));});};};DOM.parentNode = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],Fay$$fayToJs(["user","Element",[]],$p1)['parentNode']));});};DOM.children = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","NodeList",[]],Fay$$fayToJs(["user","Element",[]],$p1)['children']));});};DOM.childNodes = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","NodeList",[]],Fay$$fayToJs(["user","Element",[]],$p1)['childNodes']));});};DOM.nodeListToArray = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["list",[["user","Element",[]]]],Array.prototype.slice.call(Fay$$fayToJs(["user","NodeList",[]],$p1)));});};DOM.nodesBetween = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Element",[]]]],(function(start, end) {  var i, contents, result = [], parentNode = start.parentNode;  if(parentNode !== end.parentNode) return;  contents = Array.prototype.slice.call(parentNode.childNodes);  for(i=contents.indexOf(start); i<contents.indexOf(end); i++) {    result.push(contents[i]);  }})(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));});};};DOM.removeNodesBetween = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(start, end) {  var i, contents, parentNode = start.parentNode;  if(parentNode !== end.parentNode) return;  contents = Array.prototype.slice.call(parentNode.childNodes);  for(i=contents.indexOf(start); i<contents.indexOf(end); i++) {    parentNode.removeChild(contents[i]);  }})(Fay$$fayToJs(["user","Element",[]],$p1), Fay$$fayToJs(["user","Element",[]],$p2))));});};};DOM.createTextNode = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['createTextNode'](Fay$$fayToJs(["user","Text",[]],$p1))));});};DOM.getTextData = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p1)['data']));});};DOM.setTextData = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['data'] = Fay$$fayToJs(["user","Text",[]],$p2)));});};};DOM.clearInnerHtml = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['innerHTML'] = ''));});};DOM.klass = new Fay$$$(function(){return DOM.addClass;});DOM.addClass = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).classList['add'](Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.removeClass = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['classList']['remove'](Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.toggleClass = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['classList']['toggle'](Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.hasClass = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['classList']['contains'](Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.setAttr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['setAttribute'](Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["user","Text",[]],$p3))));});};};};DOM.getAttr = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p1)['getAttribute'](Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.hasAttr = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['hasAttribute'](Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.getValue = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Element",[]],$p1)['value']));});};DOM.setValue = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['value'] = Fay$$fayToJs(["user","Text",[]],$p2)));});};};DOM.isChecked = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_bool(Fay$$fayToJs(["user","Element",[]],$p1)['checked']));});};DOM.setChecked = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['checked'] = Fay$$fayToJs_bool($p2)));});};};DOM.getRadioValue = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],(function(name) {  var i, rs = document.getElementsByName(name);  if(rs) {    for(var i=0; i<rs.length; i++) {      var radio = rs[i];      if(radio.type === 'radio' && radio.checked)        return radio.value;    }  }})(Fay$$fayToJs(["user","Text",[]],$p1))));});};DOM.setRadioValue = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(name, val) {  var i, rs = document.getElementsByName(name);  if(rs) {    for(var i=0; i<rs.length; i++) {      var radio = rs[i];      if(radio.type === 'radio' && radio.value === val)        radio.checked = true;    }  }})(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2))));});};};DOM.getCurrentUrl = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],window['location']['href']));});DOM.logS = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],console['log'](Fay$$fayToJs(["user","Text",[]],$p1))));});};DOM.logF = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],console['log'](Fay$$fayToJs(["unknown"],$p1))));});};DOM.setInterval = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Timer",[]],(function (f,i) { var id = window['setInterval'](function () { f(id); }, i); return id; })(Fay$$fayToJs(["function",[["user","Timer",[]],["action",[["unknown"]]]]],$p2),Fay$$fayToJs_double($p1))));});};};DOM.clearInterval = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['clearInterval'](Fay$$fayToJs(["user","Timer",[]],$p1))));});};DOM.setTimeout = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Timer",[]],(function (f,i) { var id = window['setTimeout'](function () { f(id); }, i); return id; })(Fay$$fayToJs(["function",[["user","Timer",[]],["action",[["unknown"]]]]],$p2),Fay$$fayToJs_double($p1))));});};};DOM.clearTimeout = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['clearTimeout'](Fay$$fayToJs(["user","Timer",[]],$p1))));});};DOM._GET = function GET(){};DOM._GET.prototype.instance = "GET";DOM.GET = new Fay$$$(function(){return new DOM._GET();});DOM._POST = function POST(){};DOM._POST.prototype.instance = "POST";DOM.POST = new Fay$$$(function(){return new DOM._POST();});DOM._PUT = function PUT(){};DOM._PUT.prototype.instance = "PUT";DOM.PUT = new Fay$$$(function(){return new DOM._PUT();});DOM._HEAD = function HEAD(){};DOM._HEAD.prototype.instance = "HEAD";DOM.HEAD = new Fay$$$(function(){return new DOM._HEAD();});DOM._UNSENT = function UNSENT(){};DOM._UNSENT.prototype.instance = "UNSENT";DOM.UNSENT = new Fay$$$(function(){return new DOM._UNSENT();});DOM._OPENED = function OPENED(){};DOM._OPENED.prototype.instance = "OPENED";DOM.OPENED = new Fay$$$(function(){return new DOM._OPENED();});DOM._HEADERS_RECEIVED = function HEADERS_RECEIVED(){};DOM._HEADERS_RECEIVED.prototype.instance = "HEADERS_RECEIVED";DOM.HEADERS_RECEIVED = new Fay$$$(function(){return new DOM._HEADERS_RECEIVED();});DOM._LOADING = function LOADING(){};DOM._LOADING.prototype.instance = "LOADING";DOM.LOADING = new Fay$$$(function(){return new DOM._LOADING();});DOM._DONE = function DONE(){};DOM._DONE.prototype.instance = "DONE";DOM.DONE = new Fay$$$(function(){return new DOM._DONE();});DOM.xmlHttpRequest = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","XMLHttpRequest",[]],(function(window) { if(window['XMLHttpRequest']) return new XMLHttpRequest(); else return new ActiveXObject('Microsoft.XMLHTTP'); })(window)));});DOM.open = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","XMLHttpRequest",[]],(function(method, url, xhr) { xhr['open'](method['instance'], url, true); return xhr; })(Fay$$fayToJs(["user","RequestMethod",[]],$p1), Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["user","XMLHttpRequest",[]],$p3))));});};};};DOM.send = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['send']()));});};DOM.setReadyStateHandler = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","XMLHttpRequest",[]],(function(handler, xhr) { xhr['onreadystatechange'] = function() { handler(xhr); }; return xhr; })(Fay$$fayToJs(["function",[["user","XMLHttpRequest",[]],["action",[["unknown"]]]]],$p1), Fay$$fayToJs(["user","XMLHttpRequest",[]],$p2))));});};};DOM.readyState = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","ReadyState",[]],{ instance: ['UNSENT', 'OPENED', 'HEADERS_RECEIVED', 'LOADING', 'DONE'][Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['readyState']] }));});};DOM.responseText = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['responseText']));});};DOM.status = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_int(Fay$$fayToJs(["user","XMLHttpRequest",[]],$p1)['status']));});};DOM.parseInt = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_int(parseInt(Fay$$fayToJs(["user","Text",[]],$p1), 10)));});};DOM.scrollIntoView = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).scrollIntoView()));});};DOM.scrollRelative = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window.scrollBy(0,Fay$$fayToJs_int($p1))));});};DOM.scrollAbsolute = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window.scrollTo(0,Fay$$fayToJs_int($p1))));});};Fay$$objConcat(Fay$$fayToJsHash,{"GET": function(type,argTypes,_obj){var obj_ = {"instance": "GET"};return obj_;},"POST": function(type,argTypes,_obj){var obj_ = {"instance": "POST"};return obj_;},"PUT": function(type,argTypes,_obj){var obj_ = {"instance": "PUT"};return obj_;},"HEAD": function(type,argTypes,_obj){var obj_ = {"instance": "HEAD"};return obj_;},"UNSENT": function(type,argTypes,_obj){var obj_ = {"instance": "UNSENT"};return obj_;},"OPENED": function(type,argTypes,_obj){var obj_ = {"instance": "OPENED"};return obj_;},"HEADERS_RECEIVED": function(type,argTypes,_obj){var obj_ = {"instance": "HEADERS_RECEIVED"};return obj_;},"LOADING": function(type,argTypes,_obj){var obj_ = {"instance": "LOADING"};return obj_;},"DONE": function(type,argTypes,_obj){var obj_ = {"instance": "DONE"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"GET": function(type,argTypes,obj){return new DOM._GET();},"POST": function(type,argTypes,obj){return new DOM._POST();},"PUT": function(type,argTypes,obj){return new DOM._PUT();},"HEAD": function(type,argTypes,obj){return new DOM._HEAD();},"UNSENT": function(type,argTypes,obj){return new DOM._UNSENT();},"OPENED": function(type,argTypes,obj){return new DOM._OPENED();},"HEADERS_RECEIVED": function(type,argTypes,obj){return new DOM._HEADERS_RECEIVED();},"LOADING": function(type,argTypes,obj){return new DOM._LOADING();},"DONE": function(type,argTypes,obj){return new DOM._DONE();}});var Client = {};Client.FFI = {};Client.FFI.parse = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["automatic"],JSON.parse(Fay$$fayToJs(["user","Text",[]],$p1))));});};Client.FFI.json = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Text",[]],JSON.stringify(Fay$$fayToJs(["automatic"],$p1))));});};Client.FFI.attachToElemById = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(eid,val){ document.getElementById(eid).innerHTML=val })(Fay$$fayToJs(["user","Text",[]],$p1),Fay$$fayToJs(["user","Text",[]],$p2))));});};};Client.FFI.attachToParentById = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(eid,val){ document.getElementById(eid).parentElement.innerHTML=val })(Fay$$fayToJs(["user","Text",[]],$p1),Fay$$fayToJs(["user","Text",[]],$p2))));});};};Client.FFI.onClick = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).onclick=Fay$$fayToJs(["action",[["unknown"]]],$p2)));});};};Client.FFI.onKeyUp = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).onkeyup=Fay$$fayToJs(["action",[["unknown"]]],$p2)));});};};Client.FFI.onChange = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1).onchange=Fay$$fayToJs(["action",[["unknown"]]],$p2)));});};};Client.FFI.log$39$ = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],console.log(Fay$$fayToJs(["unknown"],$p1))));});};Client.main = new Fay$$$(function(){return new Fay$$$(function(){var no = new Fay$$$(function(){return function($p1){return Fay$$_(Fay$$$_return)(Fay$$unit);};});return Fay$$_(Fay$$_(Fay$$bind)(WebSockets.getWsUrl))(function($p1){var url = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(WebSockets.websocket)(url))(WebSockets.onOpen))(Client.onMessage$39$))(no))(no)))(function($p1){var ws = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.$_void))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(DOM.setTimeout)(1000)))(function($p1){return Fay$$_(Fay$$_(Client.sendAny)(ws))(Bridge.AskEvents);}));});});});});Client.onMessage$39$ = function($p1){return new Fay$$$(function(){var evt = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(WebSockets.target)(evt)))(function($p1){var ws = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(WebSockets.eventData)(evt)))(function($p1){var responseText = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Client.FFI.parse)(responseText)))(function($p1){var response = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Bridge._EmptyCmd) {return Fay$$_(Fay$$$_return)(Fay$$unit);}if (Fay$$_($tmp1) instanceof Bridge._ExecuteClient) {var cid = Fay$$_($tmp1).slot1;var task = Fay$$_($tmp1).slot2;var strategy = Fay$$_($tmp1).slot3;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(strategy))(Bridge.ExecuteAll)) ? Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Prelude.forM_)(Fay$$_(Bridge.executeRenderHtml)(task))))(function($p1){var html = $p1;return (function($tmp2){if (Fay$$_($tmp2) instanceof Bridge._AttachText) {var eid = Fay$$_($tmp2).slot1;var val = Fay$$_($tmp2).slot2;return Fay$$_(Fay$$_(Client.FFI.attachToElemById)(eid))(val);}if (Fay$$_($tmp2) instanceof Bridge._AttachDOM) {var eid = Fay$$_($tmp2).slot1;var val = Fay$$_($tmp2).slot2;return Fay$$_(Fay$$_(Client.FFI.attachToParentById)(eid))(val);}return (function(){ throw (["unhandled case",$tmp2]); })();})(html);})))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Prelude.forM_)(Fay$$_(Bridge.executeAction)(task))))(function($p1){var act = $p1;return Fay$$_(Fay$$_(Client.addListener)(ws))(act);})) : Fay$$_(Fay$$$_return)(Fay$$unit);}return (function(){ throw (["unhandled case",$tmp1]); })();})(response);});});});});};Client.addListener = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof Bridge._CallbackAction) {var eh = Fay$$_($p2).slot1;var ws = $p1;return Fay$$_(Fay$$_(Client.handle)(ws))(eh);}throw ["unhandled case in addListener",[$p1,$p2]];});};};Client.handle = function($p1){return function($p2){return new Fay$$$(function(){var eh = $p2;var ws = $p1;return (function($tmp1){if (Fay$$_($tmp1) instanceof Bridge._OnClick) {var act1 = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$_(Client.handleAction)(ws))(act1))(Client.FFI.onClick);}if (Fay$$_($tmp1) instanceof Bridge._OnKeyUp) {var act2 = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$_(Client.handleAction)(ws))(act2))(Client.FFI.onKeyUp);}if (Fay$$_($tmp1) instanceof Bridge._OnValueChange) {var act3 = Fay$$_($tmp1).slot1;return Fay$$_(Fay$$_(Fay$$_(Client.handleAction)(ws))(act3))(Client.FFI.onChange);}return Fay$$_(Client.FFI.log$39$)("not implemented yet");})(eh);});};};Client.handleAction = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var fun = $p3;if (Fay$$_($p2) instanceof Bridge._Action) {var e = Fay$$_($p2).slot1;var a = Fay$$_($p2).slot2;var c = Fay$$_($p2).slot3;var ws = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(DOM.getElementById)(e)))(function($p1){var elem = $p1;return (function(){return new Fay$$$(function(){var f = new Fay$$$(function(){return fun;});return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(f)(elem)))((function($tmp1){if (Fay$$_($tmp1) instanceof Bridge._RecordAction) {return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(DOM.getValue)(elem)))(function($p1){var val = $p1;return Fay$$_(Fay$$_(Client.sendAny)(ws))(Fay$$_(Bridge.Send)(Fay$$_(Fay$$_(Fay$$_(Bridge.Action)(e))(Bridge.RecordAction))(val)));});}if (Fay$$_($tmp1) instanceof Bridge._ObjectAction) {return Fay$$_(Fay$$_(Client.sendAny)(ws))(Fay$$_(Bridge.Send)(Fay$$_(Fay$$_(Fay$$_(Bridge.Action)(e))(Bridge.ObjectAction))(c)));}return (function(){ throw (["unhandled case",$tmp1]); })();})(a));});})();});}throw ["unhandled case in handleAction",[$p1,$p2,$p3]];});};};};Client.sendAny = function($p1){return function($p2){return new Fay$$$(function(){var val = $p2;var ws = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Client.FFI.json)(val)))(function($p1){var js = $p1;return Fay$$_(Fay$$_(WebSockets.sendWS)(ws))(js);});});};};Fay$$_(Client.main, true);
