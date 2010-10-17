function Animal()
{
	this.name = "unknown";
}

Animal.prototype.toString = function () { "an Animal";}

Animal.prototype.toString() == "an Animal";

//.toString = function() { this.name; }
/*
function Pet()
{
	this.owner="unknown"
}

Pet.prototype = new Animal();

mypet = new Pet();
*/