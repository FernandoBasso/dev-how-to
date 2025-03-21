/**
 * Adds the numeric results of the two promises.
 *
 * @param {Promise<number>} p1
 * @param {Promise<number>} p2
 * @returns {number}
 */
async function addTwoPromises(p1, p2) {
  const [res1, res2] = await Promise.all([p1, p2]);
  return res1 + res2;
}

export { addTwoPromises };
