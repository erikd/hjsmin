function figureMethodName(prototype, method)
{

  for (var key in prototype)
  {
    if ( prototype.hasOwnProperty(key) &&
        prototype[key] === method)
      return key
  }
  return null
}

function Pet()
{
	this.name = "cat";

}
