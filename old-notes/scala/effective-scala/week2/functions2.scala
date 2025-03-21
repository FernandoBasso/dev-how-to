package functions2

//
// This time with explicitly type annotations for everything.
//

val inc: Int => Int =
  (x: Int) => x + 1

val add: (Int, Int) => Int =
  (x: Int, y: Int) => x + y

//
// The type signature can also have named params, even though they
// don't need to match the params of the actual function expression.
//
val mult: (x: Int, y: Int) => Int =
  (a: Int, b: Int) => a * b

@main def main(): Unit =
  println(add(1, 1))
  println(inc(0))
  println(add(2, inc(1)))

//
// Functions are *values* (methods are not). Functions can be passed
// as arguments, returned from methods and/or other functions, stored
// in variables, arrays, etc.
//
// In Scala, are values are objects. Since functions are values, they
// are also objects.
//
