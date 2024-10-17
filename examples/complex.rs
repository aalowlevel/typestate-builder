#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

fn main() {}

#[derive(TypestateBuilder)]
struct StructWithFunctionPointer<T, F>
where
    F: Fn(T) -> T,
{
    func: F,
    value: T,
}
