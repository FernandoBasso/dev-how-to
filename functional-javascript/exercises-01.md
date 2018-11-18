# Exercises

- [noLessThan10](#noLessThan10)

## noLessThan10

Write a function that produce its argument if it is more than 10, or always produce 10 if the argument is less than 10. Try to use `when`, `lt` and `__`.

Signature: `noLessThan10 :: Number -> Number`

Solution 1:

```js
const { __, lt, when, always } = require('ramda');

/**
 * noLessThan10 :: Number -> Number
 */
const noLessThan10 = num =>
  when(lt(__, 10), always(10))(num);

log(noLessThan10(5));
// → 10
log(noLessThan10(11));
// → 11
```

