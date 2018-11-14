# Basic Functional Programming Tips

## Case 1 - logging
```js
const {
  forEach
} = require('ramda');

const log = console.log.bind(console);
```

You want to loop over a list of stuff, and console.log that stuff. People often do this:

```js
forEach(value => log(value), ['x', 'y', 'z']);
```

When they could simply have done:

```js
forEach(log, ['x', 'y', 'z']);
```

In the first example, we are _unnecessarily creating a new function for each element in the list_. With the second approach, we make the most of the fact that ECMAScript has Higher Order Functions, and we can simply pass a function that already exists and already does something we need, in this case, printing to the console.

## Case 2 - doubling


_Don't_ create an anonymous function that takes an argument and calls `double` on that argument. This is _not_ optimal:

```js
const notSoGod = map(num => double(num), [1, 2, 3]);
log(notSoGod);
// → [2, 4, 6]
```

Simply do this instead:

```js
const better = map(double, [1, 2, 3]);
log(better);
// → [2, 4, 6]
```

Again, the last version _does not_ create an unnecessary function for each element. We simply use the existing function directly.
