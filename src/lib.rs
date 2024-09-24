// Copyright (c) 2024 Andy Allison
//
// Licensed under either of
//
// * MIT license (LICENSE-MIT or http://opensource.org/licenses/MIT)
// * Apache License, Version 2.0 (LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)
//
// at your option.
//
// Unless you explicitly state otherwise, any contribution intentionally submitted
// for inclusion in the work by you, as defined in the Apache-2.0 license, shall
// be dual licensed as above, without any additional terms or conditions.

#![warn(missing_docs)]

/*! Derive-macro-based generator that combines `Typestate` and `Builder` patterns. */

use typestate_builder_macro::TypestateBuilder;

#[derive(Debug, TypestateBuilder)]
pub struct Person<'a> {
    name: &'a str,
    age: u32,
    email: Option<String>,
}

#[cfg(test)]
mod tests {
    use typestate_builder_macro::TypestateBuilder;

    #[test]
    fn builder_typestate() {
        #[derive(Debug)]
        struct Person {
            name: String,
            age: u32,
            email: Option<String>,
        }

        #[derive(Debug)]
        struct PersonBuilderNameAdded(String);
        #[derive(Debug)]
        struct PersonBuilderNameEmpty;
        #[derive(Debug)]
        struct PersonBuilderAgeAdded(u32);
        #[derive(Debug)]
        struct PersonBuilderAgeEmpty;
        #[derive(Debug)]
        struct PersonBuilderEmailAdded(Option<String>);
        #[derive(Debug)]
        struct PersonBuilderEmailEmpty;

        #[derive(Debug)]
        struct PersonBuilder<Name, Age, Email> {
            name: Name,
            age: Age,
            email: Email,
        }

        impl Person {
            fn builder(
            ) -> PersonBuilder<PersonBuilderNameEmpty, PersonBuilderAgeEmpty, PersonBuilderEmailEmpty>
            {
                PersonBuilder {
                    name: PersonBuilderNameEmpty,
                    age: PersonBuilderAgeEmpty,
                    email: PersonBuilderEmailEmpty,
                }
            }
        }

        // Add "constructor"'s
        impl<Age, Email> PersonBuilder<PersonBuilderNameEmpty, Age, Email> {
            fn name(self, name: String) -> PersonBuilder<PersonBuilderNameAdded, Age, Email> {
                PersonBuilder {
                    name: PersonBuilderNameAdded(name),
                    age: self.age,
                    email: self.email,
                }
            }
        }

        impl<Name, Email> PersonBuilder<Name, PersonBuilderAgeEmpty, Email> {
            fn age(self, age: u32) -> PersonBuilder<Name, PersonBuilderAgeAdded, Email> {
                PersonBuilder {
                    name: self.name,
                    age: PersonBuilderAgeAdded(age),
                    email: self.email,
                }
            }
        }

        impl<Name, Age> PersonBuilder<Name, Age, PersonBuilderEmailEmpty> {
            fn email(
                self,
                email: Option<String>,
            ) -> PersonBuilder<Name, Age, PersonBuilderEmailAdded> {
                PersonBuilder {
                    name: self.name,
                    age: self.age,
                    email: PersonBuilderEmailAdded(email),
                }
            }
        }

        // Build time
        impl PersonBuilder<PersonBuilderNameAdded, PersonBuilderAgeAdded, PersonBuilderEmailAdded> {
            fn build(self) -> Person {
                Person {
                    name: self.name.0,
                    age: self.age.0,
                    email: self.email.0,
                }
            }
        }

        // Add setter and/or getter method for a field
        impl Person {
            fn get_email(&self) -> &Option<String> {
                &self.email
            }
            fn set_email(&mut self, email: Option<String>) {
                self.email = email;
            }
        }

        let builder = Person::builder();
        println!("{:?}", builder);
        let builder = builder.name("Alice Johnson".to_string());
        println!("{:?}", builder);
        let builder = builder.age(30);
        println!("{:?}", builder);
        let builder = builder.email(None);
        println!("{:?}", builder);
        let mut person = builder.build();
        println!("{:?}", person);
        println!("{:?}", person.get_email());
        person.set_email(Some("alice@example.com".to_string()));
        println!("{:?}", person);
    }

    #[test]
    fn builder_typestate_derive() {}
}
