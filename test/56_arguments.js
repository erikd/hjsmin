
function bob()
{
        total = 0;
	for(i=0;i<arguments.length;i++)
	{
             total += arguments[i];
        }
        return total;
}


x = bob(1,2,3,4,5,6);

print("x= ", x);

x == 21;