# Composing Functions

Look at this more traditional code style:

```js
const log = console.log.bind(console);

const COUNTRY = 'JP';

const wasBornInCountry = ({ birthCountry }) => birthCountry === COUNTRY;

const wasNaturalized = ({ naturalizationDate }) => Boolean(naturalizationDate);

const isOver18 = ({ age }) => age >= 18;

const isCitizen = person => wasBornInCountry(person) || wasNaturalized(person);

const isEligibleToVote = person => isOver18(person) && isCitizen(person);

const laraCroft = {
    name: 'Lara Croft',
    birthCountry: 'UK',
    naturalizationDate: '1996-01-01',
    age: 23,
    skill: 'Archaeology',
};

log(isCitizen(laraCroft));
// → true

log(isEligibleToVote(laraCroft));
// → true

log(isEligibleToVote({ ...laraCroft, naturalizationDate: undefined }));
// → false

log(isEligibleToVote({ ...laraCroft, age: 17 }));
// → false
```

It could be rewriten as this: