//
// tags: option maybe some none
//

//
// Option with Some and None data constructors.
//

const log: Console["log"] = console.log.bind(console);

const none = Symbol('None');
type None = typeof none;

function some<T>(val: T): { val: T } {
  return { val };
}

type Some<V> = { val: V };

type Option<T> = Some<T> | None;

function inc(x: number): number {
  return x + 1;
}

function div(dividend: number, divisor: number): Option<number> {
  if (divisor === 0) return none;
  return some(dividend / divisor);
}

log(div(8, 2));
//=> { val: 4 }

log(div(8, 0));
//=> Symbol(None)

export {};
