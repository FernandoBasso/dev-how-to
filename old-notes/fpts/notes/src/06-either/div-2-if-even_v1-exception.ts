//
// tags: division function total partial
//

function isEven(x: number): boolean {
  return x % 2 === 0;
}

/**
 * A function that divides 2 by a divisor, but only if the divisor
 * is even and not zero.
 */
function div2IfEven(divisor: number): number | never {
  if (divisor === 0)
    throw Error("Cannot divide by zero.");

  if (!isEven(divisor))
    throw Error("Divisor is not even.");

  return 2 / divisor;
}

//
// div2IfEven() is not total (it is partial), because it only returns
// a value for certain inputs. It somehow tells client code why the
// useful value could not be returned, but using exceptions, which
// is not very FPish, and certainly not by _returning_ the message, but
// rather by interrupting execution of the program with an exception.
//

export {};
