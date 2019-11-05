//
// https://www.codewars.com/kata/54c27a33fb7da0db0100040e/train/typescript
//

const l = console.log.bind(console);

export function isSquare(n: number): boolean {
  return Math.sqrt(n) % 1 === 0;
};

l(isSquare(11));
// ⇒ false

l(isSquare(25));
// ⇒ true

l(isSquare(4));
// ⇒ true

