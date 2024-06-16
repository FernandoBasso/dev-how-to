export {};

const log: Console["log"] = console.log.bind(console);

function add(x: number): (y: number) => number {
  return function addY(y: number): number {
    return x + y;
  };
}

const res: number = add(1)(2);
log(res);
//=> 3
