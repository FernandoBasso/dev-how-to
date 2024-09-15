/**
 * Alias `console.log` to make it shorter to use. Why not‽
 *
 * @type {Console["log"]} log
 */
const log = console.log.bind(console);

(function main() {
  log('Hello, World!');
  log('It works! They said I was mad but it works!!');
})();
