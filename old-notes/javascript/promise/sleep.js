const log = console.log.bind(console);

async function sleep(millis) {
  return new Promise(function promised(resolve, _reject) {
    setTimeout(function timedOut() {
      return resolve('Resolved!');
    }, millis);
  });
}

sleep(512).then(log);
//
// $ node ./sleep.js
// (wait a few milliseconds)
//=> Resolved!
////
