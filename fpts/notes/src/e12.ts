//
// tags: sum recursion
//

export {};

const log: Console["log"] = console.log.bind(console);

function sum(xs: number[]): number {
  if (xs.length === 0) return 0;
  const [head, ...rest] = xs;
  return head + sum(rest);
}

log(sum([]));
//=> 0

log(sum([-1, -2, -3]));
//=> -6
