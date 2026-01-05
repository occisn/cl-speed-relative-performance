#include <float.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <time.h>

// gcc -O3 -shared fib.c -o fib.dll

static int32_t sub(int32_t n)
{
  if (n < 2)
    return n;
  return sub(n - 1) + sub(n - 2);
}

double naive_fibonacci(int32_t n)
{
  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  int32_t res = sub(n);

  clock_gettime(CLOCK_MONOTONIC, &end_time);

  double duration = (end_time.tv_sec - start_time.tv_sec) +
                    (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

  printf("F(%d) = %d (in %f s)\n", n, res, duration);
  fflush(stdout);

  return duration;
}


// end
