#!/usr/bin/env swift

// Contacts searching for mutt
// In your muttrc:
//
//   set query_command="contacts '%s'"
//   bind editor <Tab> complete-query
//

import AddressBook

let arguments = Process.arguments
let validArguments = arguments[arguments.startIndex + 1..<arguments.endIndex]
if validArguments.count == 0 {
  exit(1)
}

let addressBook = ABAddressBook.sharedAddressBook()
let searchString = " ".join(validArguments)
let comparison: ABSearchComparison = CFIndex(kABContainsSubStringCaseInsensitive.value)
let firstNameSearch = ABPerson.searchElementForProperty(kABFirstNameProperty,
                                                        label: nil,
                                                        key: nil,
                                                        value: searchString,
                                                        comparison: comparison);
let lastNameSearch = ABPerson.searchElementForProperty(kABLastNameProperty,
                                                       label: nil,
                                                       key: nil,
                                                       value: searchString,
                                                       comparison: comparison);
let emailSearch = ABPerson.searchElementForProperty(kABEmailProperty,
                                                    label: nil,
                                                    key: nil,
                                                    value: searchString,
                                                    comparison: comparison);
let comparisons = [firstNameSearch, lastNameSearch, emailSearch]
let orComparison = ABSearchElement(forConjunction: CFIndex(kABSearchOr.value), children: comparisons)
let found = addressBook.recordsMatchingSearchElement(orComparison) as [ABRecord]

if found.count == 0 {
  exit(0)
}

println("NAME\tEMAIL")
for person in found {
  let firstName = person.valueForProperty(kABFirstNameProperty) as String? ?? ""
  let lastName = person.valueForProperty(kABLastNameProperty) as String? ?? ""
  let emailsProperty = person.valueForProperty(kABEmailProperty) as ABMultiValue?
  if let emails = emailsProperty {
    for i in 0..<emails.count() {
      let email = emails.valueAtIndex(i) as String
      println("\(email)\t\(firstName) \(lastName)")
    }
  }
}
