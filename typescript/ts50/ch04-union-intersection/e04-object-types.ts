export const NAME = "e04 object types";

const log: Console["log"] = console.log.bind(console);

type Name = {
  name: string;
};

type Age = {
  age: number;
};

//
// ‘Person’ is an intersection of both ‘Name’ and ‘Age’.
// The intersection of both sets.
//
type Person = Name & Age;

const person: Person = {
  name: "Ada",
  age: 42,
};

log(person);
