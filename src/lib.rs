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
#[allow(dead_code)]
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

        fn get_node<'a>(&'a self) -> Self::Node<'a>;
        fn get_edge<'a>(&'a self) -> Self::Edge<'a>;
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
        fn display_node<'a>(&self) {
            let node = self.graph.get_node();
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
}
