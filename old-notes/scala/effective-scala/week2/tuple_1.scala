package tuple1

type Name = String
type Skill = String
type Level = Int

// A tuple of name and skill
type Jedi = (Name, Skill)

// A three-tuple of name, skill and level.
type Master = (Name, Skill, Level)

val ahsoka: Jedi = ("Ahsoka Tano", "Lightsaber")
val yoda: Master = ("Yoda", "The Force", 100)

//
// Could also use pattern matching like destructuring in EcmaScript:
//
val (ahsokaName, ahsokaSkill) = ahsoka
val (_, _, yodaLevel) = yoda

def getInfo(jedi: Jedi | Master): String =
  jedi match
    case (name, skill) =>
      s"Name: $name, skill: $skill"
    case (name, skill, level) =>
      s"Name: $name, Skill: $skill, Level: $level"

@main def main(): Unit =
  println(getInfo(ahsoka))
  println(getInfo(yoda))
  println(ahsokaName)
  println(yodaLevel)
