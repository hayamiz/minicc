int f(int n)
{
  return g(n);
}

int g(int n)
{
  return h(n) + h(n);
}

int h(int n)
{
  return i(n) + i(n);
}

int i(int n)
{
  return 1;
}
