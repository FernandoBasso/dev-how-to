//
// tags: hackerrank algorithm math formula
//

/**
 * Checks whether the two kangaroos can reach the same point or not.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {number} x1 The position of kangaroo 1.
 * @param {number} v1 The jump veolcity of kangaroo 1.
 * @param {number} x2 The position of kangaroo 2.
 * @param {number} v2 The jump veolcity of kangaroo 2.
 * @returns {"YES" | "NO"}
 */
function kangaroo(x1, v1, x2, v2) {
  if (v1 <= v2)
    return "NO";

  if ((x2 - x1) % (v1 - v2) === 0)
    return "YES";

  return "NO";
}

export { kangaroo };
