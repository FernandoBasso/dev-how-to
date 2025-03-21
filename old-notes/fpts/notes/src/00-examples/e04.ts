export {};

const log: Console["log"] = console.log.bind(console);

function toStr(v: number): string {
  return v.toString();
}

function inc(x: number): number {
  return x + 1;
}

/**
 * Applies the functions right-to left, that is, first `g`, then `f`.
 */
function compose<T, U, R>(
  f: (x: U) => R,
  g: (x: T) => U,
): (x: T) => R {
  return function composed(x: T): R {
    return f(g(x));
  };
}

/**
 * Increments `x` then returns it as a string.
 */
const incThenStr: (x: number) => string = compose(toStr, inc);

const res1: string = incThenStr(0);
log(res1, typeof res1);
//=> 1    string

function double(x: number): number {
  return x * 2;
}

/**
 * Doubles `x` then returns it as a string.
 */
const doubleThenStr: (x: number) => string = compose(toStr, double);

const res2: string = doubleThenStr(7);
log(res2, typeof res2);
//=> 14   string
