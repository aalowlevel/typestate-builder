# typestate-builder

`TypestateBuilder` is a Rust procedural macro that enables the creation of builder patterns using the typestate design pattern. This macro ensures that your structs are built in a way that enforces compile-time safety, ensuring that required fields are initialized before the struct is created.

## Table of Contents

- [Features](#features)
- [Usage](#usage)
  - [Deriving the Macro](#deriving-the-macro)
  - [Example](#example)
- [How It Works](#how-it-works)
- [Code Expanded](#code-expanded)
- [Limitations](#limitations)
- [License](#license)

## Features

- Enforced Typestate Pattern: Leveraging Rust's type system, the macro ensures that required fields are set before a struct can be created.
- No Runtime Checks: The macro generates the necessary types and performs checks at compile time, eliminating the need for `Option` or `Result` types.
- Compile-Time Safety: All required fields must be initialized before creating an instance of the struct, promoting safer code.
- Support for Named (and Tuple on the way) Structs: Works with named structs, with error messages for unsupported configurations.
- Fluent and Intuitive Syntax: Offers a simple and idiomatic Rust syntax for creating builders, enhancing code readability and usability.

## Example

Here’s a basic example demonstrating how to use the `TypestateBuilder` macro:

```rust
use typestate_builder::TypestateBuilder;

#[derive(Debug, TypestateBuilder)] // `Debug` is not a must.
struct Person {
    name: String,
    age: u32,
    email: Option<String>,
}

let person = Person::builder()
    .name("Alice Johnson".to_string())
    .age(30)
    .email(Some("alice@example.com".to_string()))
    .build();
println!("Created person: {:?}", person);
```

In this example, the `Person` struct uses the `TypestateBuilder` derive macro to create a `builder`. Each field can be set in a fluent interface style, and the build method assembles the `Person` instance once all required fields have been set.

## How It Works

- State Management: The macro generates intermediate state structs for each field of the struct being built. Each state struct represents a stage in the building process.

- Method Chaining: For each field, a method is generated that accepts a value for that field and returns a new builder state, ensuring that only valid transitions are allowed.

- Final Assembly: The `build` method assembles the final struct once all required fields have been set, preventing the creation of incomplete instances.

## Code Expanded

The expanded version of the above code is like this:

```rust
struct PersonBuilderNameAdded(String);
struct PersonBuilderNameEmpty;
struct PersonBuilderAgeAdded(u32);
struct PersonBuilderAgeEmpty;
struct PersonBuilderEmailAdded(Option<String>);
struct PersonBuilderEmailEmpty;
pub struct PersonBuilder<Name, Age, Email> {
    name: Name,
    age: Age,
    email: Email,
}
impl Person {
    pub fn builder() -> PersonBuilder<
        PersonBuilderNameEmpty,
        PersonBuilderAgeEmpty,
        PersonBuilderEmailEmpty,
    > {
        PersonBuilder {
            name: PersonBuilderNameEmpty,
            age: PersonBuilderAgeEmpty,
            email: PersonBuilderEmailEmpty,
        }
    }
}
impl<Age, Email> PersonBuilder<PersonBuilderNameEmpty, Age, Email> {
    pub fn name(
        self,
        name: String,
    ) -> PersonBuilder<PersonBuilderNameAdded, Age, Email> {
        PersonBuilder {
            name: PersonBuilderNameAdded(name),
            age: self.age,
            email: self.email,
        }
    }
}
impl<Name, Email> PersonBuilder<Name, PersonBuilderAgeEmpty, Email> {
    pub fn age(self, age: u32) -> PersonBuilder<Name, PersonBuilderAgeAdded, Email> {
        PersonBuilder {
            name: self.name,
            age: PersonBuilderAgeAdded(age),
            email: self.email,
        }
    }
}
impl<Name, Age> PersonBuilder<Name, Age, PersonBuilderEmailEmpty> {
    pub fn email(
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
impl PersonBuilder<
    PersonBuilderNameAdded,
    PersonBuilderAgeAdded,
    PersonBuilderEmailAdded,
> {
    pub fn build(self) -> Person {
        Person {
            name: self.name.0,
            age: self.age.0,
            email: self.email.0,
        }
    }
}
```

## Limitations

In fact, i really can't know (and no one will ever know) where is the final frontier of this crate. But i pasted [dozens complex structs here](https://github.com/aalowlevel/typestate-builder/blob/master/src/lib.rs) to have them as for setting goals, not all of them can be expected to pass the tests. Therefore, it's impossible to give a guarantee in a universe where black can be found blacker: Just don't get too complicated OR send me a PR :').

## License

`TypestateBuilder` is dual-licensed under the MIT and Apache 2.0 licenses. See the LICENSE-MIT and LICENSE-APACHE files for details.
