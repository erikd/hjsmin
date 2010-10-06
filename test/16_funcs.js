// Anonymous functions can be recursive by used the 'callee' property on the arguments object
// From http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Functions:arguments:callee

function makeFactorialFunc() {
   return function(x) {
      if (x <= 1)
         return 1;
      return x * arguments.callee(x - 1);
   };
}

var result = makeFactorialFunc()(5); // returns 120 (5 * 4 * 3 * 2 * 1)

result == 120;