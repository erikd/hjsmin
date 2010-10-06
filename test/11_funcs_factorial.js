function fac(y)
{
  if(y)
      return (fac(y-1)*y);
  else
      return 1;
}

fac(3) == 6;