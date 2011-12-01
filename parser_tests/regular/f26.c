
int f(int n)
{
  return g(g(1,2,3),g(4,5,6),g(7,8,9));
}

int g(int a, int b, int c)
{
  return a + b + c;
}
