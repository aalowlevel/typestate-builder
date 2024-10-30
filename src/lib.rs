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

/*!
# typestate-builder

`TypestateBuilder` is a Rust procedural macro that enables the creation of builder patterns using the typestate design pattern. This macro ensures that your structs are built in a way that enforces compile-time safety, ensuring that required fields are initialized before the struct is created.

## Table of Contents

- [Features](#features)
- [Usage](#usage)
  - [Deriving the Macro](#deriving-the-macro)
  - [Example](#example)
- [How It Works](#how-it-works)
- [Code Expanded](#code-expanded)
- [Possible Limitations](#possible-limitations)
- [License](#license)

## Features

- Enforced Typestate Pattern: Leveraging Rust's type system, the macro ensures that required fields are set before a struct can be created.
- No Runtime Checks: The macro generates the necessary types and performs checks at compile time, eliminating the need for Option or Result types.
- Compile-Time Safety: All required fields must be initialized before creating an instance of the struct, promoting safer code.
- Support for Named and Tuple Structs: Works seamlessly with both named and tuple structs, with clear error messages for unsupported configurations.
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

In this example, the Person struct uses the `TypestateBuilder` derive macro to create a builder. Each field can be set in a fluent interface style, and the build method assembles the `Person` instance once all required fields have been set.

## How It Works

- State Management: The macro generates intermediate state structs for each field of the struct being built. Each state struct represents a stage in the building process.

- Method Chaining: For each field, a method is generated that accepts a value for that field and returns a new builder state, ensuring that only valid transitions are allowed.

- Final Assembly: The `build` method assembles the final struct once all required fields have been set, preventing the creation of incomplete instances.

- Graph analysis: This crate internally analyzes the sub-elements of structures with graph and describes the relationships between them. Thus, the source of the final generated code is derived from this analysis.

## Code Expanded

The expanded version of the above code is like this:

```rust
struct PersonBuilder<NameGenericParam, AgeGenericParam, EmailGenericParam> {
    name: NameGenericParam,
    age: AgeGenericParam,
    email: EmailGenericParam,
}
struct PersonBuilderNameEmpty;
struct PersonBuilderNameAdded(String);
struct PersonBuilderAgeEmpty;
struct PersonBuilderAgeAdded(u32);
struct PersonBuilderEmailEmpty;
struct PersonBuilderEmailAdded(Option<String>);
impl Person {
    fn builder() -> PersonBuilder<
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
impl<
    AgeGenericParam,
    EmailGenericParam,
> PersonBuilder<PersonBuilderNameEmpty, AgeGenericParam, EmailGenericParam> {
    fn name(
        self,
        name: String,
    ) -> PersonBuilder<PersonBuilderNameAdded, AgeGenericParam, EmailGenericParam> {
        PersonBuilder {
            name: PersonBuilderNameAdded(name),
            age: self.age,
            email: self.email,
        }
    }
}
impl<
    NameGenericParam,
    EmailGenericParam,
> PersonBuilder<NameGenericParam, PersonBuilderAgeEmpty, EmailGenericParam> {
    fn age(
        self,
        age: u32,
    ) -> PersonBuilder<NameGenericParam, PersonBuilderAgeAdded, EmailGenericParam> {
        PersonBuilder {
            name: self.name,
            age: PersonBuilderAgeAdded(age),
            email: self.email,
        }
    }
}
impl<
    NameGenericParam,
    AgeGenericParam,
> PersonBuilder<NameGenericParam, AgeGenericParam, PersonBuilderEmailEmpty> {
    fn email(
        self,
        email: Option<String>,
    ) -> PersonBuilder<NameGenericParam, AgeGenericParam, PersonBuilderEmailAdded> {
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
    fn build(self) -> Person {
        Person {
            name: self.name.0,
            age: self.age.0,
            email: self.email.0,
        }
    }
}
```

## Possible Limitations

In fact, I’m not entirely sure what the upper limit is for the most complex struct that this crate can handle. However, I’ve added [dozens of complex structs here](https://github.com/aalowlevel/typestate-builder/blob/master/src/lib.rs) for testing purposes, and the crate successfully handles all of them. If you have any new ideas for testing structs, feel free to send me a PR.

## License

`TypestateBuilder` is dual-licensed under the MIT and Apache 2.0 licenses. See the LICENSE-MIT and LICENSE-APACHE files for details.
Derive-macro-based generator that combines `Typestate` and `Builder` patterns. */

pub use typestate_builder_macro::TypestateBuilder;

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::{
        alloc::{GlobalAlloc, Layout},
        collections::HashMap,
        marker::PhantomData,
    };

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

    #[derive(TypestateBuilder)]
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

    #[derive(TypestateBuilder)]
    struct MultiBoundGeneric<T, U>
    where
        T: Clone + Default + std::fmt::Debug,
        U: Into<String> + Copy,
    {
        item1: T,
        item2: U,
        data: Vec<T>,
    }

    trait Processor {
        type Input;
        type Output;

        fn process(&self, input: Self::Input) -> Self::Output;
    }

    #[derive(TypestateBuilder)]
    struct StructWithAssociatedTypes<T>
    where
        T: Processor,
    {
        processor: T,
        input: T::Input,
    }

    trait Container {
        type Item;

        fn add_item(&mut self, item: Self::Item);
        fn get_item(&self) -> &Self::Item;
    }

    #[derive(TypestateBuilder)]
    struct GenericContainer<T>
    where
        T: Container,
    {
        container: T,
    }

    #[derive(TypestateBuilder)]
    struct AdvancedPhantomStruct<'a, T>
    where
        T: 'a + Clone,
    {
        reference: &'a T,
        marker: PhantomData<T>,
    }

    trait Drawable {
        fn draw(&self);
    }

    #[derive(TypestateBuilder)]
    struct GenericWithTraitObject<'a, T>
    where
        T: 'a + Drawable,
    {
        drawable_item: Box<dyn Drawable + 'a>,
        generic_item: T,
    }

    #[derive(TypestateBuilder)]
    struct StructWithFunctionPointer<T, F>
    where
        F: Fn(T) -> T,
    {
        func: F,
        value: T,
    }

    trait Graph {
        type Node<'a>
        where
            Self: 'a;
        type Edge<'a>
        where
            Self: 'a;

        fn get_node(&self) -> Self::Node<'_>;
        fn get_edge(&self) -> Self::Edge<'_>;
    }

    #[derive(TypestateBuilder)]
    struct GraphStruct<T>
    where
        T: Graph,
    {
        graph: T,
    }

    impl<T> GraphStruct<T>
    where
        T: Graph,
    {
        fn display_node(&self) {
            // let node = self.graph.get_node();
            // Implement some logic to display the node
        }
    }

    #[derive(TypestateBuilder)]
    struct ArrayWrapper<T, const N: usize> {
        items: [T; N],
    }

    impl<T, const N: usize> ArrayWrapper<T, N> {
        fn new(items: [T; N]) -> Self {
            ArrayWrapper { items }
        }

        fn get_length(&self) -> usize {
            N
        }
    }

    trait Action {
        fn execute(&self);
    }

    #[derive(TypestateBuilder)]
    struct Dispatcher<T: Action> {
        handler: Box<dyn Action>,
        generic_handler: T,
    }

    impl<T: Action> Dispatcher<T> {
        fn run(&self) {
            self.handler.execute();
            self.generic_handler.execute();
        }
    }

    #[derive(TypestateBuilder)]
    struct NestedGenerics<'a, T, U>
    where
        T: 'a + Copy + Clone,
        U: 'a + AsRef<T> + Clone,
    {
        value: &'a T,
        ref_container: U,
    }

    impl<'a, T, U> NestedGenerics<'a, T, U>
    where
        T: 'a + Copy + Clone,
        U: 'a + AsRef<T> + Clone,
    {
        fn get_value(&self) -> T {
            *self.value
        }
    }

    #[derive(TypestateBuilder)]
    struct Tree<'a, T, U>
    where
        T: 'a + Clone,
        U: 'a + Clone,
    {
        value: &'a T,
        children: Vec<Tree<'a, U, T>>,
    }

    impl<'a, T, U> Tree<'a, T, U>
    where
        T: 'a + Clone,
        U: 'a + Clone,
    {
        fn new(value: &'a T) -> Self {
            Tree {
                value,
                children: Vec::new(),
            }
        }

        fn add_child(&mut self, child: Tree<'a, U, T>) {
            self.children.push(child);
        }
    }

    #[derive(TypestateBuilder)]
    struct ComplexGraph<'a, N, E>
    where
        N: 'a,
        E: 'a,
    {
        nodes: Vec<N>,
        edges: Vec<(N, N, E)>,
        _marker: PhantomData<&'a ()>,
    }

    #[derive(TypestateBuilder)]
    struct OptionWrapper<T, const IS_SOME: bool> {
        value: Option<T>,
    }

    impl<T> OptionWrapper<T, true> {
        fn new(value: T) -> Self {
            OptionWrapper { value: Some(value) }
        }

        fn get(&self) -> &T {
            self.value.as_ref().unwrap()
        }
    }

    impl<T> OptionWrapper<T, false> {
        fn new_none() -> Self {
            OptionWrapper { value: None }
        }
    }

    #[derive(TypestateBuilder)]
    struct DeeplyNested<'a, 'b, T, U>
    where
        T: 'a + Copy,
        U: 'b + Clone,
    {
        level_one: &'a T,
        level_two: &'b U,
        sub_nested: Vec<&'a DeeplyNested<'a, 'b, T, U>>,
    }

    impl<'a, 'b, T, U> DeeplyNested<'a, 'b, T, U>
    where
        T: 'a + Copy,
        U: 'b + Clone,
    {
        fn new(level_one: &'a T, level_two: &'b U) -> Self {
            DeeplyNested {
                level_one,
                level_two,
                sub_nested: Vec::new(),
            }
        }

        fn add_nested(&mut self, nested: &'a DeeplyNested<'a, 'b, T, U>) {
            self.sub_nested.push(nested);
        }
    }

    #[derive(TypestateBuilder)]
    struct CustomAllocator<T, A: GlobalAlloc> {
        allocator: A,
        data: *mut T,
    }

    impl<T, A: GlobalAlloc> CustomAllocator<T, A> {
        fn allocate(&mut self) -> *mut T {
            let layout = Layout::new::<T>();
            unsafe { self.allocator.alloc(layout) as *mut T }
        }

        fn deallocate(&mut self, ptr: *mut T) {
            let layout = Layout::new::<T>();
            unsafe { self.allocator.dealloc(ptr as *mut u8, layout) }
        }
    }

    #[derive(TypestateBuilder)]
    struct ComplexTuple<'a, T, U, V>(
        T,                 // Generic type T
        &'a U,             // Reference to type U with lifetime 'a
        Option<V>,         // Optional value of type V
        [T; 3],            // An array with 3 elements of type T
        fn(T, U) -> V,     // Function pointer: takes T, U, returns V
        &'a str,           // Static string slice with lifetime 'a
        Result<V, String>, // A Result type with V for success, String for error
    );
}
