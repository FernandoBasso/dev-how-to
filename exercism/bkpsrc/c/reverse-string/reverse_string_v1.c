#include <string.h>
#include <stdlib.h>
#include "reverse_string.h"

/**
 * Reverses an ASCII string.
 *
 * Uses a j variable to help move from end to beginning of dst.
 */
char *reverse(const char *src) {
  int len = strlen(src);
  char *dst = malloc(len + 1);
  char c;
  int j = len - 1;

  while ((c = *src++) != '\0')
    *(dst + j--) = c;

  *(dst + len) = '\0';

  return dst;
}
