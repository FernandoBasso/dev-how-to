export const NAME = "e04 object types";

const log: Console["log"] = console.log.bind(console);

type Name = {
  name: string;
};

type Age = {
  age: number;
};

type Person = Name & Age;

const person: Person = {
  name: "Ada",
  age: 42,
};
