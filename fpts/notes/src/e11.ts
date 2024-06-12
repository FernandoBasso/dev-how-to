const log: Console["log"] = console.log.bind(console);

function sum(xs: number[]): number {
  return (function go(acc: number, ys: number[]): number {
    if (ys.length === 0) return acc;
    return go(acc + ys[0], ys.slice(1));
  })(0, xs);
}

log(sum([]));
log(sum([-1, -2, -3]));
