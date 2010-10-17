i=1;
do
{
i++;
if(i==2) continue;
i++;
if(i==4) break;
}
while(i < 10);

for(var j=0;j<4;j++)
{
	i++;
}

print(i," ",j);


o = new Object();

o.prop1 = 1;
o.prop2 = 2;


for( var key in o) { 
	v = o[key];
	if (typeof v == "number") i += v;
}

i == 11;