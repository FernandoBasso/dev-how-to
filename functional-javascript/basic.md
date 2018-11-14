# Basic Functional Programming Tips

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

// When they could have simply done:

```js
forEach(log, ['x', 'y', 'z']);
```

In the first example, we are _unnecessarily creating a new function for each element in the list_. With the second approach, we make the most of the fact that ECMAScript has Higher Order Functions, and we can simply pass a function that already exists and already does something we need, in this case, printing to the console.
