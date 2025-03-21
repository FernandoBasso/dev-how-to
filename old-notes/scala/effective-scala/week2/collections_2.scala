package collections_2

//
// Prepend and append elements to a list.
//

val list1: List[Int] = List(2, 3)
val list2: List[Int] = 1 +: list1
val list3: List[Int] = list2 :+ 4

//
// Note we use +: and :+ 😲. Makes sense!
//

//
// Lists have order, so +: and :+ makes sense. For Map, simply +.
//

val map1: Map[String, Int] = Map(("jan", 1), "feb" -> 2)
val map2: Map[String, Int] = map1.+("mar" -> 3)
val map3: Map[String, Int] = map2 + (("apr", 4))
val map4: Map[String, Int] = map3 + ("may" -> 5)

//
// Most (if not all) operators are actually methods in Scale, similar
// to Ruby, where 1 + 1 is actually syntax sugar for 1.+(1).
//
