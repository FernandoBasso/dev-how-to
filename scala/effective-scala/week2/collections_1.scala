package collections_1

import scala.collection.mutable as m

// List[Nothing]
val list1 = List.empty

// ArrayBuffer[Nothing]
val aBuf = m.ArrayBuffer.empty

// Map[Nothing, Nothing]
val map1 = Map.empty

//
// No values can be added to those collections because of Nothing.
//
// Let's do type annotations with more useful types.
//

val list2 = List.empty[Int]
val aBuf2 = m.ArrayBuffer.empty[Double]
val map2 = Map.empty[String, Boolean]

//
// Or using vararg constructors to initialize collections with a
// variable number of arguments.
//

val list3: List[Int] = List(1, 11, 111)
val aBuf3: m.ArrayBuffer[Char] = m.ArrayBuffer('a', 'b', 'c')
val map3: Map[String, Int] = Map(
  "January" -> 1,
  "February" -> 2,
  ("March", 3),
  ("April", 4),
  // etc.
)

//
// Note to use varargs with Map we need the tuple syntax x -> y or
// (x, y). The parenthesis syntax one can create tuples with more
// than two elements.
//
