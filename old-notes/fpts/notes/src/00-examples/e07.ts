export {};

const log: Console["log"] = console.log.bind(console);

function add(x: number): (y: number) => number {
  return function addY(y: number): number {
    return x + y;
  };
}

/**
 * Like Haskell succ and pred functions!
 */
const succ: (n: number) => number = add(1);
const pred: (n: number) => number = add(-1);

log(succ(10));
//=> 11

log(pred(10));
//=> 9
