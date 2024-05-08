const isArray = Array.isArray.bind(null);
const log = console.log.bind(console);

function flat(xs, n) {
  let d = n;

  if (n === 0)
    return xs;

  function go(ys) {
    var acc = [];
    ++d;

    for (const y of ys) {
      if (isArray(y)) {
        log("array:", y);
        acc = acc.concat(y);
      }
      else {
        log("primitive:", y);
        acc.push(y);
      }
    }

    return acc;
  }

  const res = go(xs);
  log({ res });
  return res;
}

export { flat };
