import { curry2, compose2 } from "./lib";

const log: Console["log"] = console.log.bind(console);

type Option<T> = T | null;

function inc(x: number): number {
  return x + 1;
}

type MaybeNum = Option<number>;

const div = curry2(
  function div(dividend: number, divisor: number): Option<number> {
    if (divisor === 0) return null;
    return dividend / divisor;
  }
);

const divPriceBy: (x: number) => Option<number> = div(6);

const calcPriceWithTip: (x: number) => number = compose2(inc, divPriceBy);
//
// The type is still not fixed. Keep on with next examples.
////
