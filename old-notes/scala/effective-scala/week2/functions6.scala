package functions6

val xs: List[Int] = List(1, 2, 3, 4)

def gt2(x: Int): Boolean =
  x > 2

val gt2Fn: Int => Boolean =
  (x: Int) => x > 2

def filter(predicate: Int => Boolean, nums: List[Int]): List[Int] =
  nums.filter(predicate)



@main def main(): Unit =
  // NOK
  // println(xs.filter(x => x + 1))

  // OK
  println(xs.filter(x => x > 2))

  // OK
  println(xs.filter(gt2))

  // OK
  println(xs.filter(gt2Fn))
