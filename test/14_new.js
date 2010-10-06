function Animal() { this.name = "an animal"; }

o1 = new Object();
o1.name= "bob";

o2 = new Animal();

o2.name == "an animal";

o1.name == "bob" && o2.name == "an animal";