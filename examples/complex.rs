#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

#[derive(TypestateBuilder)]
pub struct MultiBoundGeneric<'a, T, const L: usize, U>
where
    T: Clone + Default + std::fmt::Debug,
    U: Into<String> + Copy,
{
    item1: T,
    item2: U,
    data: Vec<T>,
    borrowed: &'a str,
    consted: [u32; L],
}

fn main() {}
