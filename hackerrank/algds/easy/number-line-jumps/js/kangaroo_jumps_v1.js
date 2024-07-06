const MAX_POSITION_DEFAULT = 1e4;

function kangaroo(x1, v1, x2, v2, MAX_POS = MAX_POSITION_DEFAULT) {
  let p1 = x1 + v1,
      p2 = x2 + v2;

  while (p1 <= MAX_POS && p2 <= MAX_POS) {
    if (p1 === p2) return "YES";

    p1 += v1;
    p2 += v2;
  }

  return "NO";
}

export { kangaroo };
