#include <stdbool.h>
#include <stdint.h>
#include "isogram.h"

bool is_isogram(const char *s) {
  char chr,
       sub = 'x';
  uint32_t bits = 0;

  if (!s) return 0;

  while ((chr = *s++) != '\0') {
    if (chr >= 'a' && chr <= 'z')
      sub = 'a';
    else if (chr >= 'A' && chr <= 'Z')
      sub = 'A';
    else
      sub = 'x';

    if (sub == 'x')
      continue;

    if ((bits & (1 << (chr - sub))) != 0)
      return 0;
    else
      bits |= (1 << (chr - sub));
  }

  return 1;
}
