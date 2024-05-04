/**
 * Returns a value after the specified number of milliseconds.
 *
 * @param {number} millis
 * @returns {Promise<string>}
 */
async function delay(millis) {
  return new Promise(function (resolve, _reject) {
    setTimeout(() => {
      return 'Resolved!';
    }, millis);
  });
}

export { delay };
