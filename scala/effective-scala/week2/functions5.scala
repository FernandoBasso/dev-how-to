package functions5

//
// When a function uses its argument only once, we can use the so-called
// placeholder syntax style.
//

val inc: Int => Int = _ + 1

case class Contact(
  name: String,
  email: String
)

val alice = Contact("Alice", "alice@sca.la")
val bob = Contact("Bob", "bot@example.dev")

val endsWithScaDotLa: Contact => Boolean =
  _.email.endsWith("@sca.la")

//
// Placeholder arguments can be explicitly type-annotated too if needed.
//

val endsWithDotDev: Contact => Boolean =
  (_: Contact).email.endsWith(".dev")

//
// And multiple placeholders works too!
//

val add: (Int, Int) => Int = _ + _

//
// And sometimes _ is the wildcard argument. Note _ is not used. The
// function always returns 42.
//
val f: Int => Int = _ => 42

//
// Compare placeholder vs wildcard!
//
val placeholder = (_: Int) + 1
val wildcard = (_: Int) => 42

@main def main(): Unit =
  println(endsWithScaDotLa(alice))
  println(endsWithScaDotLa(bob))
  println(endsWithDotDev(bob))
  println(add(-1, 1))
