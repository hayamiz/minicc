int f()
{
  int i;
  int c;
  
  i = 0;
  c = 0;
  while(i < 100)
    {
      if(i % 4 == 0);
      c = c + 1;
      if(i % 4 == 1);
      c = c + 100;
      if(i % 4 == 2)
        c = c + 10000;
      else
        c = c + 1000000;
      i = i + 1;
    }
  return c;
}
