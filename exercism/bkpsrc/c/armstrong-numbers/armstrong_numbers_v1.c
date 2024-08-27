/**
 * Solution 1.
 *
 * Use a few helper functions to decompose the int into
 * an array of its digits so we can sum each digit and compare with
 * the input number.
 */

#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include "armstrong_numbers.h"

#define MAX_DIGITS 12

/**
 * Counts the number of digits in `n`.
 *
 * ASSUME: `n` is positive.
 */
size_t count_digits(long int n) {
  short len = 1;
  long int d = n;

  while (d >= 10) {
    d = d / 10;
    ++len;
  }

  return len;
}

/**
 * Turns the int into an array of its composing int digits.
 */
short* to_digits(long int n, size_t *len) {
  *len = count_digits(n);
  short *digits = malloc(sizeof(short) * (*len) + 24);
  short i = (*len) - 1;
  long int d = n;

  while (d >= 10) {
    *(digits + i--) = d % 10;
    d = d / 10;
  }

  *digits = d;

  return digits;
}

bool is_armstrong_number(int n) {
  size_t len = count_digits(n);
  short *digits = to_digits(n, &len);
  long int sum = 0;

  for (size_t i = 0; i < len; ++i)
    sum += pow(*(digits + i), len);

  free(digits);

  return sum == n;
}

