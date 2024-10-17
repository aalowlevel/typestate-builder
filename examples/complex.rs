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
