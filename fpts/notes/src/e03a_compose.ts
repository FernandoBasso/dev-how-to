export {};

const log: Console["log"] = console.log.bind(console);

function toStr(v: number): string {
  return v.toString();
}

function inc(x: number): number {
  return x + 1;
}

function compose(
  g: (x: number) => string,
  f: (x: number) => number,
): (x: number) => string {
  return function composed(x: number): string {
    return g(f(x));
  };
}

const incThenStr = compose(toStr, inc);

const res1: string = incThenStr(0);
log(res1, typeof res1);
//=> 1    string
