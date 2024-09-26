#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

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

fn main() {}
