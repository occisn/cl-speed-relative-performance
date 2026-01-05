#include <float.h>
#include <omp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <time.h>

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

double leibniz_1(uint64_t n)
{
  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  double tmp = 0.0;

  for (uint64_t i = 0; i < n; i++) {
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

// parallel version:
// flags: -fopenmp -march=native
double leibniz_2(uint64_t n)
{
  struct timespec start_time, end_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  double tmp = 0.0;

  const int chunk_size = n / NB_CHUNKS;

#pragma omp parallel for schedule(static, chunk_size) reduction(+ : tmp)
  for (uint64_t i = 0; i < n; i++) {
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

// end
