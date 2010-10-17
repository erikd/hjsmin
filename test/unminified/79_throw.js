myerror1 = "";

try {
	throw "Simple throw"
} 
catch (er)
{
	myerror1 = er;
}




myerror2 = ""
try {

	throw new Error("Throw in an error object")
}
catch(er)
{
	myerror2 = er.message;
}

myerror1 == "Simple throw" && myerror2 == "Throw in an error object"

