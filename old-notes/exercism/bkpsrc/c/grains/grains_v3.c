#include <stdint.h>
#include "grains.h"

#define MIN 1ull
#define MAX 64ull

/**
 * Count the number of grains in a given square.
 *
 * Using bitwise operators and avoiding loops.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 */
uint64_t square(uint8_t idx) {
  return (idx >= MIN && idx <= MAX)
    ? 1ull << (idx - 1)
    : 0ull;
}

uint64_t total(void) {
  return (((1ull << 63ull) - 1) << 1) + 1;
}
