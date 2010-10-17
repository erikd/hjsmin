// Passing function body as an argument.
// Extract from HToJS

function HSFun (name, arity, body) {
  this._n = name;                     // function name
  this._r = (arity == 0);             // evaluable only for nullary functions
  this._x = arity;                    // function arity
  this._d = this;                     // refer to self
  this._y = 0;                        // actual number of args
  this._u = null;                     // nothing up stack
  this._b = body;                     // function body
}


f = new HSFun("myfunc", 1, function() { return 99; } )

f._b.apply() == 99;