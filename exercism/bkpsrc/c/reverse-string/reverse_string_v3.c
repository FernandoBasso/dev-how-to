#include <stdio.h>
#include <stdlib.h>
#include "reverse_string.h"

/**
 * Reverse an ASCII string.
 *
 * Computes string length internally without requiring
 * strlen() from string.h.
 */
char *reverse(const char *s) {
  size_t len = 0;
  size_t j = 0;
  char *dst;

  while (*(s + len) != '\0')
    ++len;

  dst = malloc(len + 1);

  while (*(s + j) != '\0') {
    *(dst + j) = *(s + len - 1 - j);
    ++j; /* <1> */
  }

  *(dst + len) = '\0';

  return dst;
}

/* <1> We need this expression on its own to avoid
 * -Werror=sequence-point undefined behavior. See:
 *
 * • https://c-faq.com/expr/seqpoints.html
 * • https://stackoverflow.com/questions/949433/why-are-these-constructs-using-pre-and-post-increment-undefined-behavior
 * • https://stackoverflow.com/questions/26961858/operation-on-i-may-be-undefined
 */
