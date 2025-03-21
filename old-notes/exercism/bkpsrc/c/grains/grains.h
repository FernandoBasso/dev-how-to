#ifndef GRAINS_H
#define GRAINS_H

#include <stdint.h>

uint64_t for_square(uint8_t n, uint8_t i, uint64_t t);
uint64_t for_total(uint8_t i, uint64_t memo[], uint64_t acc);
uint64_t square(uint8_t index);
uint64_t total(void);

#endif
