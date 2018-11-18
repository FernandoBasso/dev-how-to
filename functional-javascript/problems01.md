# Problems 01

- [add numbers from a list](#add-numbers-from-a-list)
  + [traditional approach](#traditional-approach)
  + [functional style](#functional-style)
  + [functional style v2](#functional-style-v2)

## add numbers from a list

### traditional approach

Using a more traditional approach.

```js
// add :: [Number] -> Number
const add = nums => {
  let total = 0;
  for (let i = 0; i < nums.length; ++i) {
    total += nums[i];
  };

  return total;
}

log(add([1, 2, 3, 4]));
// → 10
```

### functional style

And what about this really functional approach? We create our `add` function _on the fly_, with the `(acc num) => acc + num` thing.

```js
const { reduce } = require('ramda');

// add :: [Number] -> Number
const addNums = nums => reduce((acc, num) => acc + num, 0, nums);

log(addNums([1, 2, 3, 4]));
// → 10
```

### functional style v2

In this case, instead of using our _on the fly add function_, we use the `add` function provided by rambda.

```js
const { reduce, add } = require('ramda');

// add :: [Number] -> Number
const addNums = nums => reduce(add, 0, nums);

log(addNums([1, 2, 3, 4]));
// → 10
```


