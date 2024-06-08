export {};

/*
type Inc = (x: number) => number;
const inc: Inc = x => x + 1;
*/

function inc(x: number): number {
  return x + 1;
}

function toStr(x: number): string {
  return x.toString();
}

function incThenToStr(x: number) {
  return toStr(inc(x));
}

//
// But this is hard-coded composition.
//
