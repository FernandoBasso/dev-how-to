//
// tags: div function total partial either
//

//
// Using never to indicate that type shouldn't occur in a given
// situation, or that it should not receive any value.
//

type Left<Error> = {
  readonly __tag: "Left";
  readonly left: Error;
};

type Right<Value> = {
  readonly __tag: "Right";
  readonly right: Value;
};

type Either<Error, Value> = Left<Error> | Right<Value>;

function left<Err, Val = never>(err: Err): Either<Err, Val> {
  return {
    __tag: "Left",
    left: err,
  };
}

function right<Val, Err = never>(val: Val): Either<Err, Val> {
  return {
    __tag: "Right",
    right: val,
  };
}

function isEven(x: number): boolean {
  return x % 2 === 0;
}

/**
 * A function that divides 2 by a divisor, but only if the divisor
 * is even and not zero.
 */
function div2IfEven(divisor: number): Either<string, number> {
  if (divisor === 0)
    left("Cannot divide by zero.");

  if (!isEven(divisor))
    left("Divisor is not even.");

  return right(2 / divisor);
}

//
// Using Either to signal the absence of a value within a Left with a
// proper error message, or a right with the useful resulting value.
//


export {};
