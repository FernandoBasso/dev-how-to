#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>
#include "isogram.h"

bool is_isogram(const char *s) {
  char seen['z' - 'a' + 1] = { 0 };
  char c;
  size_t i = 0;

  if (s == NULL) return 0;

  while ((c = tolower(s[i++]))) {
    if (!isalpha(c)) continue;

    if (seen[c - 'a']) return 0;

    seen[c - 'a'] = 1;
  }

  return 1;
}
