
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

int main(int argc, [[maybe_unused]] char* argv[argc+1]) {
  uint64_t n = 100000000; // 8 zeros
  double tmp = 0.0;
  double sign = 1.0;
  for (uint64_t i = 0; i <= n; i++) {
    tmp = tmp + sign / (2.0 * i + 1.0);
    sign = -sign;
  }
  tmp = 4 * tmp;
  printf("Result: %.20f\n", tmp);
  return EXIT_SUCCESS;
}

// Result: 3.14159266358932587337

// gcc leibniz.c -Wall -Wextra -Werror -O2 -std=c2x -pedantic -o leibniz && ./leibniz.exe

// end
