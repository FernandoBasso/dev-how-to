#include "hamming.h"

/**
 * Compute DNS hamming distance.
 *
 * - T.C: O(n)
 * - S.C: O(1)
 */
int compute(const char *lhs, const char *rhs) {
  int d = 0;

  if (!lhs || !rhs) return -1;

  for (; *lhs && *rhs; ++lhs, ++rhs)
    if (*lhs != *rhs) ++d;

  return *lhs == *rhs ? d : -1;
}
