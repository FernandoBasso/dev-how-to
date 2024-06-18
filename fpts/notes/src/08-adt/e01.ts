//
// tags: adt compose
//

type T = 1 | 2;
type U = "A" | "B" | "C";

type TUTuple = [];

type JediRecord = {
  name: string;
  level: number;
  skills: string[];
};

type JediTuple = [
  string,
  number,
  skills: string[],
];
