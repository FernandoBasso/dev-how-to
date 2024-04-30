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

    if (memo[key])
      return memo[key];

    return memo[key] = fn(...args);
  };
}

export { memoize };
