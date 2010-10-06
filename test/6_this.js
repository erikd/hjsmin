o = new Object();
o.othername = "fred";

o.f = function doit(n) { this.name = this.othername; }

// This will cause an error
//doit("bob");

o.f("bob");

o.name == "fred"