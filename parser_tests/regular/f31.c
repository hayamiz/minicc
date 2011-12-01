int f()
{
  int x;
  x = 1;
  {
    int x;
    {
      int x;
      x = 3;
    }
    x = 2;
  }
  return x;
}
