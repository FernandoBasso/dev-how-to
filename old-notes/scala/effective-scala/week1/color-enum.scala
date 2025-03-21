package ColorEnum

//
// Enums for singleton values. PrimaryColor is a type which
// contains three possible values.
//

enum PrimaryColor:
  case Red, Green, Blue

def badForColorBlindPeople(color: PrimaryColor): Boolean =
  color match
    case PrimaryColor.Red    => true
    case PrimaryColor.Green  => false
    case PrimaryColor.Blue   => true

@main def main(): Unit =
  println(badForColorBlindPeople(PrimaryColor.Green))
  println(badForColorBlindPeople(PrimaryColor.Blue))
  println("❤️")
  println(PrimaryColor.values)
  println(PrimaryColor.valueOf("Green"))
  println(PrimaryColor.valueOf(""))
