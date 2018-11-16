# Currying

```js
const log = console.log.bind(console);
```

Takes a regex and returns a function that takes a string and returns a boolean if the string matches the regex.

```js
const makeMatcher = regex => str => regex.test(str);
```

// A function that produces `true` if its string argument matches the regex `/foo/` (case insensitive).
const matchFoo = makeMatcher(/foo/i);

```js
log(matchFoo('may the force'));
// → false
log(matchFoo('Three foos and two little bar'));
// → true
```

