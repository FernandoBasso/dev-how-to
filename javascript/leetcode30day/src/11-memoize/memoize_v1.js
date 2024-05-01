/**
 * Memoizes a function.
 *
 * If a the same function reference is called again with the same input,
 * it then returns the previously computed value without running the
 * computation all over again.
 *
 * @param {Function} fn
 * @returns {Function}
 */
function memoize(fn) {
  const memo = {};

  return function memoizedFn(...args) {
    const key = JSON.stringify(...args);

    if (key in memo)
      return memo[key];

    return memo[key] = fn(...args);
  };
}

export { memoize };

//
// If the key is already in memo (our cache), then return
// the value stored there (which means fn(...args) won't
// be called).
//
// But if that key is not already there, then compute the
// result, store it in memo under that key, then return it.
//
// We used a single line compute, assign and return, which
// works just fine.
//
