/**
 * Composes two functions `f` and `g` in the form `f(g(x))`.
 */
export function compose2<T, U, R>(
  f: (v: U) => R,
  g: (v: T) => U,
): (v: T) => R {
  return function composed2(x: T): R {
    return f(g(x));
  };
}
