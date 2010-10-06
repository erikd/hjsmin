Function.prototype.inherits = function() { return 123; }

function Animal() {}

Animal.inherits() == 123;

//Animal.hasOwnProperty("bob");