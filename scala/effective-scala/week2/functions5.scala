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

@main def main(): Unit =
  println(endsWithScaDotLa(alice))
  println(endsWithScaDotLa(bob))
