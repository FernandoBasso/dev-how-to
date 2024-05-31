package SameProp_v2

enum Color:
  case Red, Green, Blue

enum Model:
  case ModelA, ModelB

case class Car(color: Color, model: Model):

def sameProps[Prop](car1: Car, car2: Car, getProp: Car => Prop): Boolean =
  getProp(car1) == getProp(car2)

def allSame(car1: Car, car2: Car): Boolean =
  ???

val car1: Car = Car(Color.Red, Model.ModelA)
val car2: Car = Car(Color.Red, Model.ModelB)
val car3: Car = Car(Color.Blue, Model.ModelA)


@main def main(): Unit =
  println(sameProps(car1, car2, _.color))
  println(sameProps(car1, car3, _.color))
