export {};

const log: Console["log"] = console.log.bind(console);

//
// A type alias just for fun.
//
type Num = number;

/**
 * A standard add function of arity 2.
 */
function add(x: Num, y: Num): Num {
  return x + y;
}

/**
 * A utility that knows how to curry a function of arity 2.
 */
function curry2(f: (x: Num, y: Num) => Num) {
  return function withArg1(a: Num): (b: Num) => Num {
    return function withArg2(b: Num): Num {
      return f(a, b);
    };
  };
}

const add2 = curry2(add);

//
// Call add2 with one param, which returns a function that takes
// the other param, which in turn returns the final result.
//
log(add2(1)(2));
//=> 3
