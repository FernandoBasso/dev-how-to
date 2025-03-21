//
// If you really want to abstract, you could take a look at lenses and a
// library for that, for example https://github.com/optics-dev/Monocle .
//
// A Lens[S, A] can be thought of as an abstraction over an attribute of
// S of type A. In your case, Lens[Car, Color] and Lens[Car, Model].
//

package SetGame3

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

//
// TODO: How to write a checkProperty() function that allows us
// to deduplicate all the functions below?
//

def sameProp[Prop](
  card1: Card,
  card2: Card,
  getProp: Card => Prop
): Boolean =
  getProp(card1) == getProp(card2)

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
