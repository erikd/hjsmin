function makeFactorialFunc(){return function(x){if(x<=1)return 1;return x*arguments.callee(x-1)}};var result=makeFactorialFunc()(5);result==120
