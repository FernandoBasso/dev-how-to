import { compose2 } from "./lib";
import { curry2 } from "./lib";

const log: Console["log"] = console.log.bind(console);

function inc(x: number): number {
  return x + 1;
}

const div = curry2(
  function div(dividend: number, divisor: number): number {
    return dividend / divisor;
  }
);

//
// Imagine we have the price of something...
//
const price: number = 42

//
// And the price is to be paid by a few people, but we yet don't know
// how many people will help pay that price. Partially apply the more
// generic `div` function to the `price`, and store the returned
// function in a more specific-named identifier.
//
const divPriceBy: (x: number) => number = div(price);

//
// Divide the price between two people.
//
log(divPriceBy(2));
//=> 21

//
// Divide the price between six people.
//
log(divPriceBy(6));
//=> 7

//
// Divide the price between zero people.
//
log(divPriceBy(0));
//=> Infinity
// Oops! This is not good. No division by zero should occur.
////
