# Allow One Function Call

- [Allow One Function Call on LeetCode](https://leetcode.com/problems/allow-one-function-call/description/?envType=study-plan-v2&envId=30-days-of-javascript).

In solution 2, we can replace `null` with `undefined`:

```javascript
fn.apply(undefined, args);
```

Also, not that comparing with solution 1, we don't spread `args` with `apply()`.

It is possible to use `Function.prototype.call` instead of `Function.prototype.apply`, in which case we again spread `args`:

```javascript
fn.call(null, ...args);
```

But not spread is used again.

`Array.prototype.apply()` takes an array as the second argument, and because our function collects arguments using rest parameter syntax, `args` becomes an array, which can simply be passed to `apply()`.

On the other hand, `Array.prototype.call()` takes multiple arguments after the first context argument (`fn.apply(null, a, b, c, etc)`), and because the function collects its arguments with rest parameter syntax which makes it an array, we have to spread it back to `call()` so it becomes “multiple arguments”.

Nowadays we can also do this:

```javascript
fn.apply(null, arguments);
```

Because after the introduction of iterables and iterators, the array-like `arguments` object **is** iterable, has a `length` property and also number indices.
But in the past, something like this was needed:

```javascript
fn.apply(null, Array.prototype.slice.call(arguments));
```

- [The arguments object on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/arguments#arguments_is_an_array-like_object).
