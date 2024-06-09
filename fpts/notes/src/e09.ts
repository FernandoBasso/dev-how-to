export {};

const log: Console["log"] = console.log.bind(console);

/**
 * A utility that knows how to curry a function of arity 2.
 */
function curry2<T, U, R>(f: (x: T, y: U) => R) {
  return function withArg1(a: T): (b: U) => R {
    return function withArg2(b: U): R {
      return f(a, b);
    };
  };
}

/**
 * A standard add function of arity 2 that adds two numbers.
 */
function add(x: number, y: number): number {
  return x + y;
}

/**
 * A standard function concat of arity 2 that concatenates two strings.
 */
function concat(s1: string, s2: string): string {
  return `${s1}${s2}`;
}

const add2 = curry2(add);
const concat2 = curry2(concat);

log(add2(1)(2));
//=> 3

log(concat2("ECMA")("Script"));
//=> ECMAScript
