//
// tags: leetcode 30dayjs javascript once apply arguments
//
// Solution using old-school approach when rest and spread
// were not yet part of the language.
//
// More examples, explanations and solutions on my Gitlab repo:
//
// https://gitlab.com/fernandobasso/dev-how-to/-/tree/devel/javascript/leetcode30day
//
//

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
  return function onceFn(/* Uses the array-like arguments object. */) {
    if (called) return undefined;

    ++called;

    ////
    // This is how we would do it in the olden days before the
    // introduction of iterators and iterables.
    //
    return fn.apply(null, Array.prototype.slice.call(arguments));
    //
    // It is like if we had done arguments.slice().
    //
    // Nowadays, even simply this suffices since (nowadays) arguments is
    // iterable, has a length property, and number indices.
    //
    //   fn.apply(null, arguments);
    ////
  };
}

export { once };
