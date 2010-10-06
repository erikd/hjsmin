// Helper to avoid Object.prototype.hasOwnProperty polluting scope objects.
// One problem is that lexer is picking the longest string
function bob()
{
  /* This a comment inside a function 
     second line of the comment */
}

function bill()
{
  /* This a comment inside a function 
     second line of the comment */
  i=2;

  function mary()
  {
    /* bob */
    var currentConstructor = this.constructor
    var methodName = null 
  }    
}
