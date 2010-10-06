
function select( fruit )
{
	switch (fruit) {
	case "Oranges":
	    s = "Oranges are $0.59 a pound.<br>";
	    break;
	case "Apples":
	    s = "Apples are $0.32 a pound.<br>";
	    break;
	case "Bananas":
	    s="Bananas are $0.48 a pound.<br>";
	    break;
	case "Cherries":
	    s="Cherries are $3.00 a pound.<br>";
	    break;
	case "Mangoes":
	case "Papayas":
	    return "Mangoes and papayas are $2.79 a pound.<br>";
	    break;
	default:
	    s="Sorry, we are out of " + fruit
	}
	return s;
}

t1 = select("Oranges") == "Oranges are $0.59 a pound.<br>"
t2 = select("Nuts") == "Sorry, we are out of Nuts"
t3 = select("Mangoes") == "Mangoes and papayas are $2.79 a pound.<br>"

t1 && t2 && t3
