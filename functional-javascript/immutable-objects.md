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

## removing properties from objects

Just to get started, let's retrieve individual properties from objects using _destructuring_ syntax.

```js
const jedi = {
  id: 1,
  name: 'Yoda',
  skill: 'The Force',
};

const { name, skill } = jedi;
log(name, skill);
// 'Yoda'
// 'The Force'
```

And we can also make an antire new object _without_ some properties of the original object by making use of *destructuring* and the *rest syntax*.

```js
const { id, ...jediNoId } = jedi; // <1>
log(jediNoId);

// [object Object] {
//   name: "Yoda",
//   skill: "The Force"
// }
```

1. This time we use the _spread syntax_ *to the left of the assignment operator*. We also _destructure_ the id (but don't care about it afterwards), which leaves all the remaining properties to end up in `jediNoId`.


