bob = [1,2,"fred"];

for(i=0;i<bob.length;i++) bob[i]=i*2;

t1 = bob[0] == 0 && bob[2] == 4 && bob.length == 3;

bob = new Array(1,2,3);
bob[0]++;

bob.push(4,5)

t2 = bob[0] == bob[1] && bob[2] == 3 && bob[3] == 4 && bob[4] == 5;

t1 && t2;
