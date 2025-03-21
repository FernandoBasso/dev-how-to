#include "hamming.h"

int compute(const char *lhs, const char *rhs) {
  int d = 0;
  char p, q;

  while (lhs && rhs) {
    p = *lhs++;
    q = *rhs++;

    if (p == '\0' && q == '\0') return d;

    if ((p == '\0' && q != '\0') || (q == '\0' && p != '\0'))
      return -1;

    if (p != q)
      ++d;
  }

  return d;
}
