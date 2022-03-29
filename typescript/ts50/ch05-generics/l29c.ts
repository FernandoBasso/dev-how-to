export const NAME = "l29c Generics in the Wild";

const log: Console["log"] = console.log.bind(console);

async function rand(): Promise<number> {
  return Math.random();
}

//
// Using generic and bracket syntax.
//
const xs: number[] = [1, 2, 3];
const ys: Array<number> = [4, 5, 6];

//
// If we want an array that may contain multiple types (union
// types), the generic syntax must be used.
//
const allThings: Array<number | string | { [key: string]: unknown }> = [
  "one",
  2,
  { 4: "four" },
  { five: 5 },
  "six",
  7,
];
