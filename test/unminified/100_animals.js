
// -- Animal --
function Animal(name)
{
  if (!name)
    throw new Error('Must specify an animal name')
  this.name = name
}

Animal.prototype.toString = function()
{
  return this.name
}

o = new Animal("bob")

o.toString() == "bob";


