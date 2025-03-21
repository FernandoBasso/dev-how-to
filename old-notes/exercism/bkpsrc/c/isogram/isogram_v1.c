#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include "isogram.h"

/* The English alphabet contains 26 letters. */
#define ALPHABET_LEN 26

/**
 * Check whether s is an isogram.
 *
 * This solution involves storing the already seen chars in an array.
 *
 * ASSUME: Lowercase chars only. Spaces and hyphens can show
 * up multiple times.
 */
bool is_isogram(const char s[]) {
  short i = 0,
        j = 0;
  char c,
       seen[ALPHABET_LEN] = { '\0' };

  if (s == NULL) return 0;
  if (strlen(s) == 0) return 1;

  while ((c = *(s + i++)) != '\0')
    if (c == ' ' || c == '-')
      continue;
    else if (strchr(seen, tolower(c)) != NULL)
      return 0;
    else
      seen[j++] = tolower(c);

  return 1;
}
