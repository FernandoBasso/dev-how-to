const log = console.log.bind(console);

/**
 * Computes the addition `x` and `y`.
 *
 * @sig Number Number -> Number
 * @param {number} x
 * @param {number} y
 * @returns {number}
 */
function add(x, y) {
  return x + y;
}

/**
 * Finds the min and max sum of the five-integer array.
 *
 * ASSUME: The input always contains five positive integers and is
 * sorted in ascending order.
 *
 * @sig [Int] -> { min: Int, max: Int }
 * @param {number} nums
 * @returns {{ min: number, max: number }}
 */
function miniMaxSum(nums) {
  let total = 0;
  let smallest = Infinity;
  let largest = -Infinity;

  for (let i = 0; i < nums.length; ++i) {
    let curNum = nums[i];

    total += curNum;

    if (curNum < smallest)
      smallest = curNum;

    if (curNum > largest)
      largest = curNum;
  }

  log({
    smallest,
    largest,
    minSum: total - largest,
    maxSum: total - smallest,
  });
}

miniMaxSum([7, 9, 2, 3, 5]);
