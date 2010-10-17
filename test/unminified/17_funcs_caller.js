function myFunc() {
   if (myFunc.caller == null) {
      return ("The function was called from the top!");
   } else
      return ("This function's caller was " + myFunc.caller.name);
}

function otherFunc()
{
	return myFunc();
}

x = myFunc()
y = otherFunc()

x == "The function was called from the top!" && y == "This function's caller was otherFunc"