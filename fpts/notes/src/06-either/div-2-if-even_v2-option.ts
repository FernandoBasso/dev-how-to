//
// tags: division function total partial option some none
//

import { type Option, none, some } from "../lib";

function isEven(x: number): boolean {
  return x % 2 === 0;
}

//
// To make div2IfEven() total, we refrain from throwing exceptions and
// instead return some and none. Client code then has to handle the
// returned value and decide what to do in each case.
//
// But we lost our error messages. We are simply indicating the absence
// of a useful value, but not the reason why we don't have a useful
// value.
//
// In case of invalid inputs, error messages are really important.
//
// We need to keep this function total, while still retaining the useful
// error messages.
//

/**
 * A function that divides 2 by a divisor, but only if the divisor
 * is even and not zero.
 */
function div2IfEven(divisor: number): Option<number> {
  if (divisor === 0 || !isEven(divisor)) return none;

  return some(2 / divisor);
}
