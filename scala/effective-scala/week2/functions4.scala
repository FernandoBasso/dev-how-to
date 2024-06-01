package functions4

//
// When a compiler needs a function, most of the time it is able to
// convert methods into functions.
//

val xs: List[Int] = List(1, 2, 3)

def incMethod(x: Int): Int =
  x + 1

def incFunction: Int => Int = (x: Int) => x + 1

@main def main(): Unit =
  //
  // These all end up producing the same List(2, 3, 4) result.
  //
  println(xs.map(x => x + 1))
  println(xs.map(_ + 1))
  println(xs.map(incMethod))
  println(xs.map(incFunction))

//
// When the compiler expects a function value, but sees a method, it
// converts the method to a function value.
//
