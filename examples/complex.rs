#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

fn main() {}

#[derive(TypestateBuilder)]
struct NestedGenerics<'a, T, U>
where
    T: 'a + Copy + Clone,
    U: 'a + AsRef<T> + Clone,
{
    value: &'a T,
    ref_container: U,
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
