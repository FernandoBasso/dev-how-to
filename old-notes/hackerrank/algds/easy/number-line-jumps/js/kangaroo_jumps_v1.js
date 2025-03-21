//
// tags: hackerrank algorithm math formula kangaroo
//

const MAX_ITERATIONS = 1e4;

/**
 * Checks whether the two kangaroos can reach the same point or not.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 *
 * @param {number} x1 The position of kangaroo 1.
 * @param {number} v1 The jump veolcity of kangaroo 1.
 * @param {number} x2 The position of kangaroo 2.
 * @param {number} v2 The jump veolcity of kangaroo 2.
 * @returns {"YES" | "NO"}
 */
function kangaroo(x1, v1, x2, v2) {
  let p1 = x1,
      p2 = x2,
      i = 0;

  while (i++ < MAX_ITERATIONS) {
    if (p1 === p2) return "YES";

    p1 += v1;
    p2 += v2;
  }

  return "NO";
}

export { kangaroo };
