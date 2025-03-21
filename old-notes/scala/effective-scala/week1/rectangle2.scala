package Rectangle2

case class Rectangle(width: Int, height: Int):
  val area: Int = width * height

val facade = Rectangle(width = 5, height = 3)

val small = Rectangle(width = 3, height = 4)

//
// Update on copy.
//
val large = small.copy(
  width = small.width * 2,
  height = small.height * 2
)

@main def main(): Unit =
  println(facade.area)
  println(Rectangle(8, 2).area)
  println(small.area)
  println(large.area)

