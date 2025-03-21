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
  let total = nums[0];
  let smallest = nums[0];
  let largest = nums[0];

  for (let i = 1; i < nums.length; ++i) {
    let curNum = nums[i];

    total += curNum;

    if (curNum < smallest)
      smallest = curNum;

    if (curNum > largest)
      largest = curNum;
  }

  return {
    min: total - largest,
    max: total - smallest,
  };
}

export { miniMaxSum };
