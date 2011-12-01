int f()
{
  int x;
  
  x = 1;
  {
    int p;
    
    p = 10;
    x = p + x;
  }
  return x;
}
