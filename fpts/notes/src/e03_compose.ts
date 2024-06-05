export {};

const log: Console["log"] = console.log.bind(console);

function compose(f, g, x) {
  return g(f(x));
}

log()
