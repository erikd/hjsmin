
function Pet() {
}

Pet.prototype.toString = function() { return "[pet]"; }

s = new String("a string");
o = new Object();
p = new Pet()

x = s.toString();
y = o.toString();
z = p.toString();

x == "a string" && y == "[object]" && z == "[pet]"


