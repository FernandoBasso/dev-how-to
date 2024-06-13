import { curry2, compose2 } from "./lib";

const log: Console["log"] = console.log.bind(console);

//
// Using branded types to create the data and value constructors.
//

type None = {
  readonly __tag: 'None';
};

const none: Option<never> = {
  __tag: 'None',
};

type Some<V> = {
  readonly __tag: 'Some';
  readonly val: V;
};

function some<V>(v: V): Option<V> {
  return {
    __tag: 'Some',
    val: v,
  };
}

type Option<T> = Some<T> | None;

function inc(x: number): number {
  return x + 1;
}

function div(dividend: number, divisor: number): Option<number> {
  if (divisor === 0) return none;
  return some(dividend / divisor);
}

log(div(8, 2));
//=> { __tag: 'Some', val: 4 }

log(div(8, 0));
//=> { __tag: 'None' }
