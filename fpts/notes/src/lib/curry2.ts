export function curry2<T, U, R>(
  f: (x: T, y: U) => R
) {
  return function fnForX(x: T): (y: U) => R {
    return function fnForY(y: U): R {
      return f(x, y);
    };
  };
}
