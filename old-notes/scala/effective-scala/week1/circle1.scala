package Circle1

case class Circle(radius: Double):
  val area: Double = radius * radius * 3.14159

@main def main(): Unit =
  println(Circle(radius = 5).area)
