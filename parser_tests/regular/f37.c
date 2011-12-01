int f()
{
  int i;
  int c;
  
  i = 0;
  c = 0;
  while(i < 100)
    {
      if(i >= 70)
        {
          i = i + 1;
          continue;
        }
      c = c + i;
      i = i + 1;
    }
  return c;
}
