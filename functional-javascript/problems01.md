# Problems 01

- [add numbers from a list](#add-numbers-from-a-list)
  + [traditional approach](#traditional-approach)
  + [functional style](#functional-style)

## add numbers from a list

### traditional approach

Using a more traditional approach.

```
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

And what about this really functional approach?

```
const { reduce } = require('ramda');

// add :: [Number] -> Number
const addNums = nums => reduce((acc, num) => acc + num, 0, nums);

log(addNums([1, 2, 3, 4]));
// → 10
```