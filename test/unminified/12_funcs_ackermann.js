function acker(m,n)
{
	if(m==0) return n+1;
        if(n==0) return acker(m-1,1);
        return acker(m-1,acker(m,n-1));
}

acker(3,2) == 29;