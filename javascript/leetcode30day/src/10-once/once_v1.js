/**
 * Allows invoking `fn` only once. Returns undefined on subsequent calls
 * of the same function reference.
 *
 * - T.C: Depends on T.C of `fn` and its parameters.
 * - S.C: Same notes as for T.C.
 *
 * @param {Function} fn
 * @returns {(args: ...unknown) => unknown}
 */
function once(fn) {
  var called = 0;

  /**
   * @param {args: ...unknown}
   * @returns {unknown}
   */
  return function onceFn(...args) {
    if (called) return undefined;

    ++called;

    return fn(...args);
  }
}

export { once };
