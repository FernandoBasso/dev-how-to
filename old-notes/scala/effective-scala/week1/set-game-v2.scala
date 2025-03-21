package SetGame2

enum Shape:
  case Diamond, Squiggle, Oval

enum Color:
  case Red, Green, Purple

enum Shading:
  case Open, Striped, Solid

enum Number:
  case One, Two, Three

case class Card(
  shape: Shape,
  number: Number,
  color: Color,
  shading: Shading,
)

def checkShapeProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.shape == card2.shape &&
    card2.shape == card3.shape
  def allDifferent =
    card1.shape != card2.shape &&
    card1.shape != card3.shape &&
    card2.shape != card3.shape
  allSame || allDifferent

def checkNumberProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.number == card2.number &&
    card2.number == card3.number
  def allDifferent =
    card1.number != card2.number &&
    card1.number != card3.number &&
    card2.number != card3.number
  allSame || allDifferent

def checkColorProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.color == card2.color &&
    card2.color == card3.color
  def allDifferent =
    card1.color != card2.color &&
    card1.color != card3.color &&
    card2.color != card3.color
  allSame || allDifferent

def checkShadingProperty(card1: Card, card2: Card, card3: Card): Boolean =
  def allSame =
    card1.shading == card2.shading &&
    card2.shading == card3.shading
  def allDifferent =
    card1.shading != card2.shading &&
    card1.shading != card3.shading &&
    card2.shading != card3.shading
  allSame || allDifferent

def isValidSet(card1: Card, card2: Card, card3: Card): Boolean =
  checkShapeProperty(card1, card2, card3) &&
  checkNumberProperty(card1, card2, card3) &&
  checkColorProperty(card1, card2, card3) &&
  checkShadingProperty(card1, card2, card3)

val deck: List[Card] = List(
  Card(Shape.Diamond,  Number.One, Color.Purple,  Shading.Striped),
  Card(Shape.Squiggle, Number.Two, Color.Red,     Shading.Open),
  Card(Shape.Oval,     Number.Three, Color.Green, Shading.Solid),
)

val deck1Valid = isValidSet(
  Card(Shape.Diamond,  Number.One, Color.Purple,  Shading.Striped),
  Card(Shape.Squiggle, Number.Two, Color.Red,     Shading.Open),
  Card(Shape.Oval,     Number.Three, Color.Green, Shading.Solid),
)

val deck2Valid = isValidSet(
  Card(Shape.Diamond,  Number.One, Color.Purple,  Shading.Striped),
  Card(Shape.Squiggle, Number.Three, Color.Red,   Shading.Open),
  Card(Shape.Oval,     Number.Three, Color.Green, Shading.Solid),
)

@main def main(): Unit =
  println(deck1Valid)
  println(deck2Valid)
