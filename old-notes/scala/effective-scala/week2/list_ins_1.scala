package list_ins_1

case class Contact(
  name: String,
  email: String,
  phoneNumbers: List[String]
)

case class AddressBook(contacts: List[Contact])

val alice = Contact("Alice", "alice@example.dev", List())
val carol = Contact("Carol", "carol@example.dev", List("+5512345678"))
val bob = Contact("Bob", "bob@example.dev", List("+5512341234"))

val contacts1: List[Contact] = List(alice, carol)

//
// Construct a new list by prepending bob to the existing list.  A new
// list is created (immutable). Scala uses the concept of persistent
// data structures. We don't copy the entire tail list, but instead,
// reused it.
//

val contacts2: List[Contact] = bob :: contacts1

//
// Equivalent to this:
//
val users: List[Contact] = bob :: alice :: carol :: Nil

//
// The :: cons operator is right-associative (like : in Haskell).
//
val users2: List[Contact] = bob :: (alice :: (carol :: Nil))

//
// And it is all methods in the end.
//
// Note the order of chaining, though.
//
val users3: List[Contact] = Nil.::(carol).::(alice).::(bob)

def printNames(users: List[Contact]): Unit =
  for user <- users do
    println(user.name)

@main def main(): Unit =
  println(contacts2)

  println()
  printNames(contacts2)

  println()
  printNames(users)

  println()
  printNames(users2)

  println()
  printNames(users3)
