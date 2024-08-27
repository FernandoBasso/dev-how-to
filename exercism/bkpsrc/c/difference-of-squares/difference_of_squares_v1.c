#include <stdlib.h>
#include "difference_of_squares.h"

/**
 * Computes the sum of 1 to num then squares it.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 */
unsigned int square_of_sum(unsigned int num) {
  unsigned int r = 0;
  size_t i;

  for (i = 1; i <= num; ++i)
    r += i;

  return r * r;
}

/**
 * Computes the sum of the squares of num.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 */
unsigned int sum_of_squares(unsigned int num) {
  unsigned int r = 0;
  size_t i;

  for (i = 1; i <= num; ++i)
    r += i * i;

  return r;
}

unsigned int difference_of_squares(unsigned int num) {
  return square_of_sum(num) - sum_of_squares(num);
}
