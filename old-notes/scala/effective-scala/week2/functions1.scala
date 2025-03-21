package functions1

//
// Functions are expressions assigned to `val` (not defined
// with def).
//
// Parameter types cannot be inferred (like in TypeScript where
// they would default to `any` type), thus, even though many
// many types can be inferred, including return types, parameters
// must be explicitly typed.
//
// I advocate for being explicit most of the time anyway, even when
// types can be inferred and annotations are not required.
//

val inc = (x: Int) => x + 1

val add = (x: Int, y: Int) => x + y

@main def main(): Unit =
  println(add(1, 2))
  println(inc(0))
  println(add(2, inc(1)))
