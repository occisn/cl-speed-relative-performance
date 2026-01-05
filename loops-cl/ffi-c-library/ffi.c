#include <float.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define n 100000 // 100 K
#define m 100000 // 100 K

static int loops(void)
{
  int sum = 0;
  const int u = 40;
  srand(time(NULL));
  const int r = rand() % 10000; // Get a random integer 0 <= r < 10k
  int32_t a[n] = {0};
  for (int i = 1; i < n; i++) {
    for (int j = 0; j < m; j++) {
      a[i] = a[i] + (j + r) % u;
    }
    a[i] += r;
    sum += a[i];
  }
  printf("sum = %d\n", sum);
  return 0;
}

double run(void)
{

  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  loops();

  clock_gettime(CLOCK_MONOTONIC, &end_time);

  double duration = (end_time.tv_sec - start_time.tv_sec) +
                    (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

  printf("Done in %f seconds\n", duration);
  fflush(stdout);

  return duration;
}

// end
