/* Copyright (c) 2006 Charlie Savage 
License: BSD:
http://www.opensource.org/licenses/bsd-license.php

As posted on http://cfis.savagexi.com/articles/2006/08/25/new-and-improved-javascript-inheritance
*/

Function.prototype.inherits = function(superConstructor)
{
  /* Borrowed from Kevin Lindsey - create a dummy
     constructor to create prototypes.  This is useful
     because it means that we don't have to modify
     the real constructors to have if-statements to 
     guard against initialization if we are creating
     a prototype (versus a regular instance).*/
  function CreatePrototype() {}
  CreatePrototype.prototype = superConstructor.prototype
  
  /* Reset the current constructor. This is an ugly hack required 
     by the JavaScript prototype-chain implementation. */
  this.prototype = new CreatePrototype()
  this.prototype.constructor = this
  
  // Save a reference to the superClass constructor
  this.superConstructor = superConstructor

  this.prototype.callSuper = function()
  {
    /* In IE and Firefox we can get the caller argument via
       arguments.callee.caller.  However, for some reason this
       has been deprecated in JavaScript and is not supported
       by Safari or Opera.  So we have to pass it in as
       a parameter.  Yuck.*/
    var caller = arguments.callee.caller
    var caller = arguments[0]
    var args = new Array()
    for (var i=1; i<arguments.length; i++)
      args.push(arguments[i])
    
     /* Figure out where we are in the inheritance hierarchy and
       the name of the method that was invoked. */
    var currentConstructor = this.constructor
    var methodName = null 
    
    while (methodName == null && currentConstructor != null)
    {
      // Shortcut - Is a constructor being called?
      if (caller === currentConstructor)
        return currentConstructor.superConstructor.apply(this, args)
  
      methodName = figureMethodName(currentConstructor.prototype, caller)
      currentConstructor = currentConstructor.superConstructor
    }
    
    if (!methodName)
      throw new Error("Could not find method: " + methodName + ".")

    if (!currentConstructor)
      throw new Error("Prototype does not have an ancestor: "
       + currentConstructor + ".")
      
    // Finally - execute the method     
    return currentConstructor.prototype[methodName].apply(this, args)
  }    
}

function figureMethodName(prototype, method)
{
  for (var key in prototype)
  {
    if (prototype.hasOwnProperty(key) &&
        prototype[key] === method)
      return key
  }
  return null
}

// ----- Example --- 

// -- Animal --
function Animal(name)
{
  if (!name)
    throw new Error('Must specify an animal name')
  this.name = name
}

Animal.prototype.toString = function()
{
  return 'My name is ' + this.name
}

// -- Pet --
function Pet(owner, name)
{
  this.owner = owner
  this.callSuper(arguments.callee, name)
}
Pet.inherits(Animal)

Pet.prototype.toString = function()
{
  return this.callSuper(arguments.callee) + "\n" + 
         "My owner is " + this.owner
}

// -- Cat --
function Cat(owner, name)
{
  this.callSuper(arguments.callee, owner, name)
}
Cat.inherits(Pet)
 
Cat.prototype.toString = function()
{
  return this.callSuper(arguments.callee) + '\n' + 
        'I eat mice'
}
        
var cat = new Cat('charlie', 'oba')

print(cat.toString())