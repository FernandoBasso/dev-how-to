#include <stdlib.h>
#include <string.h>
#include "reverse_string.h"

/**
 * Reverse an ASCII string.
 */
char *reverse(const char *src) {
  int len = strlen(src);
  char *dst = malloc(len + 1);
  char c;

  /* Point to last char just before before
   * where '\0' will later be added */
  dst += len - 1;

  while ((c = *src++) != '\0')
    *dst-- = c;

  /* At this point, dst is pointing to memory position on less than
   * its correct initial place. Advance it once so it correctly
   * points to the beginning of its allotted memory. */
  dst++;

  /* Add the string terminator at last position. */
  *(dst + len) = '\0';

  return dst;
}
