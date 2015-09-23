/* FIB -- A classic benchmark, computes fib(40) inefficiently. */

#include <stdio.h>

static int fib (n)
int n;
{
  if (n < 2)
    return n;
  else
    return fib (n-1) + fib (n-2);
}

int main (int argc, char *argv[])
{
  printf("%d\n", fib(40));

  return 0;
}
