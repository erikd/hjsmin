
o = new Object();

o.prop1 = 1;
o.prop2 = 2;
o.prop3 = 66;



function bob()
{
	o.prop3 = 3;
        return 1;
	o.prop3 = 6;
}

y=bob();

x = o.propertyNames()

f = o.hasOwnProperty("prop1") && ! o.hasOwnProperty("fred");

o.prop1 + o.prop2 + o.prop3 == 6 && f ;


