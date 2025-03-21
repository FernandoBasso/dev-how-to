//
// tags: option string
//

const log: Console["log"] = console.log.bind(console);

import {
  type Option,
  type None,
  type Some,
  isNone,
  none,
  some,
} from "../lib/option";

/**
 * Pattern-matching.
 *
 * Applies function when the value is Some, and another function when
 * the value Some.
 */
function match<I, R1, R2>(
  onNone: () => R1,
  onSome: (v: I) => R2,
): (x: Option<I>) => R1 | R2 {
  return function forX(x: Option<I>) {
    return isNone(x)
      ? onNone()
      : onSome(x.val);
  }
}

const numNone: Option<number> = none;
const num42: Option<number> = some(42);

log(match(
  () => 'None',
  (v: number) => v
)(numNone));
//=> None

log(match(
  () => 'None',
  (v: number) => v
)(num42));
//=> 42
