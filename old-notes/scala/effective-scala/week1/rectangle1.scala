package Rectangle1

case class Rectangle(width: Int, height: Int):
  val area: Int = width * height

case class Rect(w: Int, h: Int):
  def area: Int = w * h

val facade = Rectangle(width = 5, height = 3)

@main def main(): Unit =
  println(facade.area)
  println(Rectangle(8, 2).area)
  println(Rect(8, 2).area)

