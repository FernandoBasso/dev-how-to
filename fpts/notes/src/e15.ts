import { curry2, compose2 } from "./lib";

const log: Console["log"] = console.log.bind(console);

function inc(x: number): number {
  return x + 1;
}

const div = curry2(
  function div(dividend: number, divisor: number): number {
    return dividend / divisor;
  }
);

const price: number = 42

const divPriceBy: (x: number) => number = div(price);

const calcPriceWithTip = compose2(inc, divPriceBy);

log(calcPriceWithTip(6));
//=> 8
//
// 42 / 6 = 7, + 1 for tip is 8. This is correct ✅.
////

//
// But if we divide by zero, it returns Infinity, and incrementing
// Infinity by 1 is still Infinity. This is simply wrong.
//
log(calcPriceWithTip(0));
//=> Infinity
