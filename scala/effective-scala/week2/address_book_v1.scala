package address_book_v1

//
// Lists can store zero or more of a particular type of item.
// Order is maintained.
//

case class Contact(
  name: String,
  email: String,
  phoneNumbers: List[String]
)

case class AddressBook(contacts: List[Contact])

val alice = Contact("Alice", "alice@example.dev", List())
val bob = Contact("Bob", "bob@example.dev", List("+5512341234"))
val addrBook = AddressBook(List(alice, bob))

@main def main: Unit =
  println(addrBook.contacts.map(_.name))
  println(addrBook.contacts.map(_.email))
  println(addrBook.contacts.map(c => List(c.name, c.email)))
