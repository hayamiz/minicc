int f(int n)
{
  if (n == 0) return 1;
  else {
    return f(n - 1) + f(n - 1);
  }
}
