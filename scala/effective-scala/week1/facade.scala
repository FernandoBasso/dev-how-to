def house(facade: Double, window: Double): Double =
  val door: Double = 2 * 1
  val areaToSubtract: Double = door + window * 2
  facade - areaToSubtract
end house

@main def house(): Unit =
  println(house(6, 1))
