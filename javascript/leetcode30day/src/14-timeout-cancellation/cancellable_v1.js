const log = console.log.bind(console);
/**
 * @param {Function} fn
 * @param {unknown[]} args
 * @param {number} millis
 * @returns {Function}
 */
function cancellable(fn, args, millis) {
  log({ millis });
  const timeoutId = setTimeout(function timedOut() {
    log('before timeout');
    fn(...args);
  }, millis);

  return function clear() {
    clearTimeout(timeoutId);
  };
}

const cancelFn = cancellable(x => x * 2, [2], 40);

setTimeout(cancelFn, 30);


export { cancellable };
