package Shape3

sealed trait Shape
case class Rectangle(width: Int, height: Int) extends Shape
case class Circle(radius: Int) extends Shape

val rect1: Shape = Rectangle(4, 5)
val fiveByFour: Rectangle = Rectangle(5, 4)
val circle = Circle(2)

def area1(shape: Shape): Double =
  shape match
    case Rectangle(w, h) => w * h
    case Circle(r)       => r * r * 3.14159

def area2(shape: Shape): Double =
  shape match
    case rect: Rectangle => rect.width * rect.height
    case circle: Circle => circle.radius * circle.radius * 3.14

@main def main(): Unit =
  println(area1(Rectangle(5, 4)))
  println(area1(Circle(3)))
  println(area2(Rectangle(4, 5)))
  println(area2(Circle(3)))
