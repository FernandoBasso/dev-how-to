/**
 * @param {Function} fn
 * @param {unknown[]} args
 * @param {number} millis
 * @returns {Function}
 */
function cancellable(fn, args, millis) {
  fn(...args);

  const intervalId = setInterval(function onInterval() {
    fn(...args);
  }, millis);

  return function cancelInterval() {
    clearInterval(intervalId);
  };
}

export { cancellable };
