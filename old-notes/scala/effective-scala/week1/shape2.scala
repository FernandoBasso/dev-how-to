package Shape2

sealed trait Shape
case class Rectangle(width: Int, height: Int) extends Shape
case class Circle(radius: Double) extends Shape

var rect1: Shape = Rectangle(width = 2, height = 4)
val circle1: Shape = Circle(radius = 4)

def shapeDescription(shape: Shape): String =
  shape match
    case circle: Circle => s"Circle with radius “${circle.radius}”"
    // case _              => "Some non-Circle shape"

@main def main(): Unit =
  println(shapeDescription(circle1))
  println(shapeDescription(rect1))
