#include <stdio.h>
#include <stdint.h>
#include "grains.h"

#define NUM_SQUARES 64

/**
 * Doubles the squares n times. Recursive.
 *
 * ASSUME: i < n.
 *
 * - T.C: O(n)
 * - S.C: O(1)
 */
uint64_t for_square(uint8_t n, uint8_t i, uint64_t acc) {
  if (n < 1) return 0ull;

  if (n == i) return acc;

  return for_square(n, i + 1, acc * 2);
}

/**
 * Simply calls for_square() to do the job.
 */
uint64_t square(uint8_t idx) {
  return for_square(idx, 1, 1);
}

/**
 * Memoize previously computed solutions and reuse them.
 *
 * Computes each new square based on the memoized computation
 * of the previous squares.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 */
uint64_t for_total(uint8_t i, uint64_t memo[], uint64_t acc) {
  if (i == 1) {
    memo[1] = 1;
    return for_total(i + 1, memo, acc + memo[1]);
  }

  if (i == NUM_SQUARES) {
    memo[i] = memo[i - 1] * 2;
    return acc + memo[NUM_SQUARES];
  }

  memo[i] = memo[i - 1] * 2;

  return for_total(i + 1, memo, acc + memo[i]);
}

/**
 * Counts the total grains in a NUM_SQUARES square board.
 */
uint64_t total(void) {
  uint64_t memo[NUM_SQUARES];

  return for_total(1, memo, 0ull);
}
