package Square1

//
// We could define Square in terms of Rectangle.
//

case class Square(width: Int):
  val area: Int = width * width
