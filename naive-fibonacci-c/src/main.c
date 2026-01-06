#include <float.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int32_t naive_fibonacci(int32_t n)
{
  if (n < 2)
    return n;
  return naive_fibonacci(n - 1) + naive_fibonacci(n - 2);
}

__attribute__((noinline))
int32_t naive_fibonacci__not_inlined(int32_t n)
{
  if (n < 2)
    return n;
  return naive_fibonacci(n - 1) + naive_fibonacci(n - 2);
}

double naive_fibonacci_1(uint32_t n)
{

  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  const int32_t res = naive_fibonacci(n);

  clock_gettime(CLOCK_MONOTONIC, &end_time);

  double duration = (end_time.tv_sec - start_time.tv_sec) +
                    (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

  printf("Fibonacci(%" PRIu32 ") = %" PRIu32 " in %f seconds\n", n, res, duration);
  fflush(stdout);

  return duration;
}

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

void naive_fibonacci_1__benchmark_5_times(uint32_t n)
{
  const int nb_runs = 5;
  double durations[nb_runs];

  for (int i = 0; i < nb_runs; i++) {
    double duration = naive_fibonacci_1(n);
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

double naive_fibonacci_2(uint32_t n)
{

  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  const int32_t res = naive_fibonacci__not_inlined(n);

  clock_gettime(CLOCK_MONOTONIC, &end_time);

  double duration = (end_time.tv_sec - start_time.tv_sec) +
                    (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

  printf("Fibonacci(%" PRIu32 ") = %" PRIu32 " in %f seconds\n", n, res, duration);
  fflush(stdout);

  return duration;
}

void naive_fibonacci_2__benchmark_5_times(uint32_t n)
{
  const int nb_runs = 5;
  double durations[nb_runs];

  for (int i = 0; i < nb_runs; i++) {
    double duration = naive_fibonacci_2(n);
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
  const uint32_t N = 46; // result: 1836311903

  printf("\nnaive_fibonacci_1\n-----------------\n\n");
  naive_fibonacci_1(N);
  // naive_fibonacci_1__benchmark_5_times(N);

  printf("\nnaive_fibonacci_2\n-----------------\n\n");
  naive_fibonacci_2(N);
  // naive_fibonacci_2__benchmark_5_times(N);

  return EXIT_SUCCESS;
}

// end
