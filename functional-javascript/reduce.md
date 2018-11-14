# Reduce

```javascript
const {
  reduce,
  subtract,
} = require('ramda');
```

This really subtracts from the previous subtraction value. Subtract 1, from 0, then 2 from -1, then 3 from -3, which is -6.

```javascript
log(reduce(subtract, 0, [1, 2, 3]));
// → 6
```

This ends up doing 1 - 2, ignoring any remaining args.

```javascript
log(subtract(1, 2, 3));
// → -1
```
