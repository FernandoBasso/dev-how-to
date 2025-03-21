//
// tags: sum recursion
//

export {};

const log: Console["log"] = console.log.bind(console);

function sum(xs: number[]): number {
  if (xs.length === 0) return 0;
  return xs[0] + sum(xs.slice(1));
}

log(sum([]));
//=> 0

log(sum([-1, -2, -3]));
//=> -6
