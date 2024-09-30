#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

fn main() {}

#[derive(TypestateBuilder)]
struct GenericStruct<T, U>
where
    T: Copy,
    U: Copy,
{
    field1: T,
    field2: U,
}
