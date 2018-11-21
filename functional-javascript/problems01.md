# Problems 01

- [add numbers from a list](#add-numbers-from-a-list)
  - [traditional approach](#traditional-approach)
  - [functional style](#functional-style)
  - [functional style v2](#functional-style-v2)
- [filter todos](#filter-todos)

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

## filter todos

```js
const {
  filter,
  where,
  equals,
} = require('ramda');

// task: { id: number, done: boolean, text: string, due: string yyyy-mm-dd }

// tasks: [todo]
const tasks = [
  { id: 1, done: false, text: 'Watch the Alien Movies Again', due: '2000-01-01' },
  { id: 2, done: true, text: 'Learn JavaScript', due: '2000-01-03' },
  { id: 3, done: false, text: 'Learn They Hindley Milner Type System', due: '2000-01-01' },
  { id: 4, done: false, text: 'Review Sed Exercises', due: '2000-01-02' },
  { id: 5, done: true, text: 'Update Arch Linux', due: '2000-01-03' },
];

const getDoneTasks = filter(where({ done: equals(true) })); // <1>
log(getDoneTasks(tasks));
// → [ { id: 2, done: true, text: 'Learn JavaScript', due: '2000-01-03' },
// →   { id: 5, done: true, text: 'Update Arch Linux', due: '2000-01-03' } ]
```

1. `filter` and `where` are curried. They way we used them is still lacking one argument: the data to work on. Therefore, intead of actually really filtering where `done` is `true`, we just return a function and store it in `getDoneTasks`. This function is expecting the data/tasks argument so actuall filtering can take place.

