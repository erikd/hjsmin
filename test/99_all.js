
obj = new Object();

function bob(i)
{
    do {
	if(i==1) obj.prop1 = 1; else obj.prop1=2;
        if(i==1) bob(2);
        break; 
        obj.prop1=99;
    } while(false);
}

bob(1);
obj.prop1 == 2;
       