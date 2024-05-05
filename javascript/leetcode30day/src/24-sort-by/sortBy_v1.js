/**
 * Returns a function that sorts a and b in ascending order.
 *
 * @param {Function} getValueToSortBy
 * @returns {(a: number, b: number) => number}
 */
function sortAscBy(getValueToSortBy) {
  return function sortAB(a, b) {
    return getValueToSortBy(a) - getValueToSortBy(b);
  };
}

/**
 * Sorts the array in assending order using fn() to determine
 * the values to sort by.
 *
 * @param {Array<unknown>} arr
 * @param {Function} getValueToSortBy
 * @returns {Array<unknown>}
 */
function sortBy(arr, getValueToSortBy) {
  return arr.sort(sortAscBy(getValueToSortBy));
}

export { sortBy };

//
// We could replace
//
//   arr.sort(sortAscBy(getValueToSortBy))
//
// with something like
//
//   return arr.sort((a, b) => getValueToSortBy(a) - getValueToSortBy(b));
//
