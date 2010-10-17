
function verifyZipCode( zip )
{
	pattern = /[0-9]{5}([- ]?[0-9]{4})?/;
	return pattern.test(zip);
}


a = verifyZipCode(95060);         // T
b = verifyZipCode(9560);          // F
c = verifyZipCode("a");           // F
d = verifyZipCode("95060");       // T
e = verifyZipCode("95060 1234");  // T

a && !b  && !c && d && e

