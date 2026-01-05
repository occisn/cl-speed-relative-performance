#include <float.h>
#include <omp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 10000000000 // 10 M
#define NB_THREADS 8
#define NB_CHUNKS (4 * NB_THREADS)

int compare_double(const void *a, const void *b)
{
  double arg1 = *(const double *)a;
  double arg2 = *(const double *)b;
  if (arg1 < arg2)
    return -1;
  if (arg1 > arg2)
    return 1;
  return 0;
}

double leibniz_1()
{
  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  double tmp = 0.0;

  for (uint64_t i = 0; i < N; i++) {
    double sign = (i % 2 == 0) ? 1.0 : -1.0;
    tmp += sign / (2.0 * i + 1.0);
  }

  tmp *= 4.0;

  clock_gettime(CLOCK_MONOTONIC, &end_time);

  double duration = (end_time.tv_sec - start_time.tv_sec) +
                    (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

  printf("Leibniz formula: pp = %.20f (in %f s)\n",
         tmp, duration);
  fflush(stdout);

  return duration;
}

void leibniz_1__benchmark_5_times()
{
  const int nb_runs = 5;
  double durations[nb_runs];

  for (int i = 0; i < nb_runs; i++) {
    double duration = leibniz_1();
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

// parallel version:
// flags: -fopenmp -march=native
double leibniz_2()
{
  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  double tmp = 0.0;

  const int chunk_size = N / NB_CHUNKS;

#pragma omp parallel for schedule(static, chunk_size) reduction(+ : tmp)
  for (uint64_t i = 0; i < N; i++) {
    double sign = (i & 1) ? -1.0 : 1.0;
    tmp += sign / (2.0 * i + 1.0);
  }

  tmp *= 4.0;

  clock_gettime(CLOCK_MONOTONIC, &end_time);

  double duration = (end_time.tv_sec - start_time.tv_sec) +
                    (end_time.tv_nsec - start_time.tv_nsec) / 1e9;

  printf("Leibniz formula: pi = %.20f (in %f s)\n",
         tmp, duration);
  fflush(stdout);

  return duration;
}

void leibniz_2__benchmark_5_times()
{
  const int nb_runs = 5;
  double durations[nb_runs];

  for (int i = 0; i < nb_runs; i++) {
    double duration = leibniz_2();
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
  printf("\n\nLEIBNIZ 1\n---------\n\n");         
  leibniz_1__benchmark_5_times();

  printf("\n\nLEIBNIZ 2\n---------\n\n");         
  leibniz_2__benchmark_5_times();

  return EXIT_SUCCESS;
}

// end
