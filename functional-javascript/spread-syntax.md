# Spread Syntax

[intro](#intro)
[copy object](#copy-object)
[copy object and overwrite property](#copy-object-and-overwrite-property)
[copy object and add new property to it](#copy-object-and-add-new-property-to-it)


## intro
Spread syntax is useful for many things. One of them is to help us with immutable data.

```js
const log = console.log.bind(console);
```

## copy object

TODO


## copy object and add new property to it

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

## copy object and overwrite property

We can also copy an object, overwriting any of its original properties.

```js
const yoda3 = {
  ...yoda2,
  skill: 'Teach the ways of the force', // <1>
};

log(yoda3);
// [object Object] {
//   id: 1,
//   name: "Master Yoda",
//   skill: "Teach the ways of the force"
// }
```

1. `yoda2` already has a `skill` property, but we have overwritten the old value with a new value.


