package address_book_v1

//
// Lists can store zero or more of particular type of item.
// Order is maintained.
//

case class Contact(
  name: String,
  email: String,
  phoneNumbers: List[String]
)

case class AddressBook(contacts: List[Contact])
