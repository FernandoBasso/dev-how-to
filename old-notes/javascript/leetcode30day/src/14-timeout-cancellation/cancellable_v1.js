/**
 * @param {Function} fn
 * @param {unknown[]} args
 * @param {number} millis
 * @returns {Function}
 */
function cancellable(fn, args, millis) {
  const timeoutId = setTimeout(function timedOut() {
    fn(...args);
  }, millis);

  return function clear() {
    clearTimeout(timeoutId);
  };
}

export { cancellable };
