package list_first_1

case class Contact(
  name: String,
  email: String,
  phoneNumbers: List[String]
)

case class AddressBook(contacts: List[Contact])

val alice = Contact("Alice", "alice@example.dev", List())
val carol = Contact("Carol", "carol@example.dev", List("+5512345678"))
val bob = Contact("Bob", "bob@example.dev", List("+5512341234"))

val contacts: List[Contact] = List(alice, carol)

def getFirst(contacts: List[Contact]): Option[Contact] =
  contacts match
    case head :: tail => Some(head)
    case Nil          => None

def getSecond(contacts: List[Contact]): Option[Contact] =
  contacts match
    case _ :: second :: _ => Some(second)
    case _                => None

//
// Using Nil to pattern-match only matches empty list. _ is the
// wildcard thing with matches anything.
//

@main def main(): Unit =
  println(getFirst(contacts))
  println(getSecond(contacts))
