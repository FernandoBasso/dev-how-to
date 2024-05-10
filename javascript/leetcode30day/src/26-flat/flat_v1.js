const log = console.log.bind(console);

const isArray = Array.isArray.bind(null);

function flat(xs, n) {
  let d = 1;

  if (n === 0 || !xs.length)
    return xs;

  let acc = [];

  function go(ys) {
    for (const y of ys) {
      if (isArray(y)) {
        if (n >= d) {
          ++d;
          go(y);
        }
        else
          // acc = acc.concat(y);
        acc.push(y);
      }
      else {
        acc.push(y);
      }
    }

    return acc;
  }

  const res = go(xs);
  return res;
}

export { flat };
