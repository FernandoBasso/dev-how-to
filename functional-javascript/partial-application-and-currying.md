# Partial Application and Currying

- [filtering book list by date](#filtering-book-list-by-date)


## filtering book list by date
```js
const { filter, map, prop} = require('ramda');

const bookList = [
  { title: 'Lord of The Rings', year: 1954 },
  { title: 'The Silence Of The Lambs', year: 1988 },
  { title: 'The Little Prince', year: 1943 },
];

// const publishedInYear = ({ year }, targetYear ) => year === targetYear;

/**
 * Produce `true` if the book `year` is the same as `targetYear`.
 *
 * Curried!
 *
 * publishedInYear :: Year -> Book -> Boolean
 */
const publishedInYear = targetYear => ({ year }) => year === targetYear;


/**
 * Produce a list of titles of books published on a given year.
 *
 * titlesForyear :: List-Of-Book, Year -> List-Of-Book
 */
const titlesForyear = (targetYear, books) => {
  const selectedBooks = filter(publishedInYear(targetYear), books); // <1>
  return map(prop('title'), selectedBooks); // <2>
};

log(titlesForyear(1988, bookList));
// → [ 'The Silence Of The Lambs' ]
```

1. Since `publishedInYear` is curried, we pass it `targetYear`, and `filter` takes care of passing it a `book`, the “missing” argument.
2. `map` takes care of passing the `book` to `prop`. We only pass `prop` the property we want to retrieve.


