#include <stdint.h>
#include "grains.h"

/**
 * Counts how many grains there are in square idx.
 *
 * - T.C: O(n)
 * - S.C: O(1)
 */
uint64_t square(uint8_t idx) {
  uint64_t sq = 1;
  uint8_t i;

  if (idx < 1) return 0ull;

  for (i = 1; i < idx; ++i)
    sq += sq;

  return sq;
}

/**
 * Counts the total grains in a 64 square board.
 *
 * - T.C: O(n²). To compute square 3, we recompute square 1 and 2.
 *               To compute square 4, we recompute square 3, 2 and 1.
 *               There is probably a way to memoize this.
 * - S.C: O(1)
 */
uint64_t total(void) {
  uint64_t t = 0ull;
  uint8_t i;

  for (i = 1; i <= 64; ++i)
    t += square(i);

  return t;
}
