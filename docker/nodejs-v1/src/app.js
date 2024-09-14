/**
 * Alias `console.log` to make it shorter to use. Why not‽
 *
 * @type {Console["log"]} log
 */
const log = console.log.bind(console);

/**
 * Returns a hello message to a given person's name.
 *
 * @param {string} name The name of the user for the hello message.
 * @returns {string}
 */
function getHello(name) {
  return `Hello, ${name}!`;
}

/**
 * Returns the actual parameters passed to the node process.
 *
 * Node's `process.argv` returns the node executable and the script
 * being run as the first two values. We want to skip those and
 * return only the params actually provided on the command line.
 *
 * @param {NodeJS.Process["argv"]} argv
 * @returns {Array<string>}
 */
function getParams(argv) {
  return argv.slice(2);
}

/**
 * Logs a hello message personalized to the given user's name.
 *
 * @param {name} string
 */
function sayHello(name) {
  log(getHello(name));
}

/**
 * Boot the program.
 */
(function main() {
  const params = getParams(process.argv);

  sayHello(params[0]);
})();
