#include <float.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define n 100000 // 100 K
#define m 100000 // 100 K

static int compare_double(const void *a, const void *b)
{
  double arg1 = *(const double *)a;
  double arg2 = *(const double *)b;
  if (arg1 < arg2)
    return -1;
  if (arg1 > arg2)
    return 1;
  return 0;
}

static int loops(void)
{
  int sum = 0;
  const int u = 40;
  srand(time(NULL));     
  const int r = rand() % 10000;      // Get a random integer 0 <= r < 10k
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

void benchmark_5_times(void)
{
  const int nb_runs = 5;
  double durations[nb_runs];

  for (int i = 0; i < nb_runs; i++) {
    double duration = run();
    printf("Run %d / %d: %f seconds\n", i + 1, nb_runs, duration);
    fflush(stdout);
    durations[i] = duration;
  }

  qsort(durations, nb_runs, sizeof(double), compare_double);
  double quickest = durations[0];
  double second_best = durations[1];
  double slowest = durations[nb_runs - 1];

  printf("\nRESULTS:\n");
  for (int i = 0; i < nb_runs; i++) {
    printf("Run %d / %d: %f seconds\n", i + 1, nb_runs, durations[i]);
  }
  printf("=> quickest execution: %f seconds\n", quickest);
  printf("=> second best:        %f seconds\n", second_best);
  printf("=> slowest execution:  %f seconds = quickest + %ld %%\n",
         slowest, (long)(100.0 * (slowest - quickest) / quickest));
}

int main(void)
{
  // run();
  benchmark_5_times(); // 5.5s
  return EXIT_SUCCESS;
}

// end
