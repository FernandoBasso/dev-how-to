const l = console.log.bind(console, '====');

const testsPassed = (text = 'All Tests Passed!') => {
  const len = text.length + 10;
  console.info(`${'='.repeat(len)}\n==== ${text} ====\n${'='.repeat(len)}`);
}

export {
  l,
  testsPassed,
};
