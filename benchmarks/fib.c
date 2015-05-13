/* FIB -- A classic benchmark, computes fib(35) inefficiently. */

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
  printf("%d\n", fib(35));

  return 0;
}
