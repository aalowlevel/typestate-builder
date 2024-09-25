# typestate-builder

`TypestateBuilder` is a Rust procedural macro that enables the creation of builder patterns using the typestate design pattern. This macro ensures that your structs are built in a way that enforces compile-time safety, ensuring that required fields are initialized before the struct is created.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
  - [Deriving the Macro](#deriving-the-macro)
  - [Example](#example)
- [How It Works](#how-it-works)
- [License](#license)

## Features

- The Typestate Pattern is the king in Rust!
- No runtime checks : Creates the necessary types and just does its job.. Thus, there is no need for `Option` or `Result` types..
- Safety at compile time : Ensures that all required fields are set before creating an instance of the struct.
- Supports named and tuple structs (panics on else).
- Simple, intuitive and rusty-way syntax for creating builders.

## Installation

To use `TypestateBuilder`, add the following to your `Cargo.toml`:

```toml
[dependencies]
typestate-builder = "0.1"  # Replace with the latest version
```

## Example

Here’s a basic example demonstrating how to use the `TypestateBuilder` macro:

```rust
use typestate_builder::TypestateBuilder;

#[derive(TypestateBuilder)]
struct Person {
    name: String,
    age: u32,
    email: Option<String>,
}

fn main() {
    let person = Person::builder()
    .name("Alice Johnson".to_string())
    .age(30)
    .email(Some("alice@example.com".to_string()))
    .build();

    println!("Created person: {:?}", person);
}
```

In this example, the Person struct uses the `TypestateBuilder` derive macro to create a builder. Each field can be set in a fluent interface style, and the build method assembles the Person instance once all required fields have been set.

## How It Works

- State Management: The macro generates intermediate state structs for each field of the struct being built. Each state struct represents a stage in the building process.

- Method Chaining: For each field, a method is generated that accepts a value for that field and returns a new builder state, ensuring that only valid transitions are allowed.

- Final Assembly: The `build` method assembles the final struct once all required fields have been set, preventing the creation of incomplete instances.

## Limitations

In fact, i really can't know (and no one will ever know) where is the final frontier of this crate. But i pasted [dozens complex structs here](https://github.com/aalowlevel/typestate-builder/blob/master/src/lib.rs) to have them as for setting goals, thus, not all of them can be expected to pass the tests.

## License

`TypestateBuilder` is dual-licensed under the MIT and Apache 2.0 licenses. See the LICENSE-MIT and LICENSE-APACHE files for details.
