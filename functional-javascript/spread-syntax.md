# Spread Syntax

[intro](#intro)

## intro
Spread syntax is useful for many things. One of them is to help us with immutable data.

```js
const log = console.log.bind(console);
```

## add prop to object

Create an object with two properties.

```js
const yoda1 = {
  id: 1,
  name: 'Master Yoda',
};

log(yoda1);
// [object Object] {
//   id: 1,
//   name: "Master Yoda"
// }
```

Create a new object which has all the properties of the previous object, and adds a new `skill` property.

```js
const yoda2 = {
  ...yodav1, // <1>
  skill: 'Foresight',
};

log(yodav2);
// [object Object] {
//   id: 1,
//   name: "Master Yoda",
//   skill: "Foresight"
// }

```

1. Note the use of the spread syntax *to the right* of the _assignment operator_. In that position, it “expands” the properties of the object, which are added to the new object being created.

