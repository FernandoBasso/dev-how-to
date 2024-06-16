export {};

const log: Console["log"] = console.log.bind(console);

function add(x: number, y: number): number {
  return x + y;
}

log(add(1, 2));
//=> 3
