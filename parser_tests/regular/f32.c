int f()
{
  int x;
  int y;
  x = 0;
  y = 1;
  {
    x = x + y;
    {
      x = x + y;
      {
        x = x + y;
      }
    }
  }
  return x;
}
