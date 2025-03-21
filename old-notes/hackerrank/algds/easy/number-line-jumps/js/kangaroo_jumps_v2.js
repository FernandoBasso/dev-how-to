//
// tags: hackerrank algorithm math formula
//

/**
 * Checks whether the two kangaroos can reach the same point or not.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {number} p1 The position of kangaroo 1.
 * @param {number} v1 The jump velocity of kangaroo 1.
 * @param {number} p2 The position of kangaroo 2.
 * @param {number} v2 The jump velocity of kangaroo 2.
 * @returns {"YES" | "NO"}
 */
function kangaroo(p1, v1, p2, v2) {
  if (v1 <= v2)
    return "NO";

  if ((p2 - p1) % (v1 - v2) === 0)
    return "YES";

  return "NO";
}

export { kangaroo };
