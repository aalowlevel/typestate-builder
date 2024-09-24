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

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, marker::PhantomData};

    use typestate_builder_macro::TypestateBuilder;

    #[derive(TypestateBuilder)]
    struct BasicStructWithLifetimes<'a> {
        name: &'a str,
        age: u32,
        description: &'a str,
    }

    #[derive(TypestateBuilder)]
    struct GenericStruct<T, U>
    where
        T: Copy,
        U: Copy,
    {
        field1: T,
        field2: U,
    }

    #[derive(TypestateBuilder)]
    struct InnerStruct {
        x: i32,
        y: i32,
    }

    #[derive(TypestateBuilder)]
    struct OuterStruct {
        name: String,
        inner: InnerStruct,
        active: bool,
    }

    /* Panic: TypestateBuilder only supports structs */
    // #[derive(TypestateBuilder)]
    enum Status {
        Active,
        Inactive,
        Suspended,
    }

    #[derive(TypestateBuilder)]
    struct EnumWithinStruct {
        user_id: u64,
        status: Status,
    }

    #[derive(TypestateBuilder)]
    struct StructWithOptionals {
        title: Option<String>,
        description: Option<String>,
        rating: Option<u8>,
    }

    /* Panic: Not yet implemented */
    // #[derive(TypestateBuilder)]
    struct TupleStruct(i32, f64, String, Option<u8>);

    #[derive(TypestateBuilder)]
    struct ComplexStruct {
        name: String,
        values: Vec<i32>,
        map: HashMap<String, u64>,
    }

    #[derive(TypestateBuilder)]
    struct RecursiveStruct {
        value: i32,
        next: Option<Box<RecursiveStruct>>,
    }

    #[derive(TypestateBuilder)]
    struct PhantomStruct<T> {
        data: i32,
        marker: PhantomData<T>,
    }

    /* Panic: May only be applied tostruct's. */
    // #[derive(TypestateBuilder)]
    trait MyTrait {
        fn do_something(&self);
    }

    #[derive(TypestateBuilder)]
    struct StructWithCustomTrait {
        name: String,
        value: u32,
    }

    impl MyTrait for StructWithCustomTrait {
        fn do_something(&self) {
            println!("Doing something with {}", self.name);
        }
    }

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
}
