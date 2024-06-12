//
// tags: sum recursion tail-call
//

export {};

const log: Console["log"] = console.log.bind(console);

function sum(nums: number[]): number {
  return (function go(acc: number, xs: number[]): number {
    if (xs.length === 0) return acc;
    const [x, ...restOfXs] = xs;
    return go(acc + x, restOfXs);
  })(0, nums);
}

log(sum([]));
//=> 0

log(sum([-1, -2, -3]));
//=> -6
