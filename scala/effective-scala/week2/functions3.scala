package functions3

//
// If functions are values, and in Scala are values are objects, then it
// follows that functions are also objects. And objects have methods.
// Therefore, functions have methods.
//
// Calling a function means calling its apply() method. The compiler
// rewrites function call() syntax to fn.apply(param) syntax internally.
//
// Any object that has an apply() method can be short-handed to call
// like simple function call syntax.
//

val inc: Int => Int =
  (x: Int) => x + 1

@main def main(): Unit =
  println(inc(41))
  println(inc.apply(41))
