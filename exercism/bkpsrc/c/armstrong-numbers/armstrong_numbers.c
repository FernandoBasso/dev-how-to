/**
 * Solution 2.
 *
 * No helper functions and compute the power using an inner for loop.
 *
 * Get the number of digits by using the log10(n) + 1 math trick.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include "armstrong_numbers.h"

#define MAX_DIGITS 12

bool is_armstrong_number(int n) {
  int len = log10(n) + 1,
      total = 0,
      pow_tmp = 1,
      d = n,
      r;

  if (d < 10) return 1;

  while (d > 0) {
    r = d % 10;

    pow_tmp = 1;

    for (int i = 0; i < len; ++i)
      pow_tmp *= r;

    total += pow_tmp;

    r = d % 10;
    d = d / 10;
  }

  return total == n;
}
