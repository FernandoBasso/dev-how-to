#include <stdlib.h>
#include "difference_of_squares.h"

/**
 * Computes the sum of 1 to num then squares it.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 */
unsigned int square_of_sum(unsigned int n) {
  unsigned int sum = n * (n + 1) / 2;
  return sum * sum;
}

/**
 * Computes the sum of the squares of num.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 */
unsigned int sum_of_squares(unsigned int n) {
  return n * (n + 1) * (n * 2 + 1) / 6;
}

unsigned int difference_of_squares(unsigned int num) {
  return square_of_sum(num) - sum_of_squares(num);
}
