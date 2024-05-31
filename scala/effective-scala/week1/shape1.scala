package Shape1

//
// Sealed traits are more or less like an abstract
// class or interface. They exist mainly as a supertype.
//

sealed trait Shape
case class Rectangle(width: Int, height: Int) extends Shape
case class Circle(radius: Double) extends Shape

var rect1: Shape = Rectangle(width = 2, height = 4)
val circle1: Shape = Circle(radius = 4)

//
// Shape has no members. We need to match and recover the
// concrete type, extract the members, and perform the
// computation accordingly.
//

def getArea(shape: Shape): Int | Double =
  shape match
    case Rectangle(width, height) => width * height
    case Circle(radius)           => radius * radius * 3.14159

//
// getArea() is not a total function if it is forgetting to
// handle other shapes (like Triangle, etc.), in which case
// no case would match.
//

@main def main(): Unit =
  println(rect1)
  println(circle1)
  println(getArea(rect1))
  println(getArea(circle1))
