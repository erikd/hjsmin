
function bob(n)
{
    try {
	throw n; // throws an exception with a numeric value
    } catch (e) {
	if (e <= 50) {
	    return true;
	} else {
	    // cannot handle this exception, so rethrow
	    throw (e);
	}
    }
}

function fred(n)
{
    try {
	return bob(n);
    } 
    catch(e)
    {
	return false;
    }
}


bob(11) && ! fred(1234);

	

