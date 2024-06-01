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
