package tuple_2

val point = (4, -3)
val yoda = ("Yoda", "The Force", 100)

//
// Can access by index (Haskell has fst, snd). Scala can access
// any tuple index.
//
val x = point(0)
val y = point(1)

val level = yoda(2)

@main def main(): Unit =
  println(x)
  println(y)
  println(level)
