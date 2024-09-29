#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

fn main() {}

pub(crate) trait MyTrait {}

#[derive(TypestateBuilder)]
pub(crate) struct MultiBoundGeneric<'a, T, B, const L: usize, U, V>
where
    T: Clone + Default + std::fmt::Debug,
    U: Into<String> + Copy,
    V: MyTrait,
{
    item1: T,
    item2: U,
    data: Vec<T>,
    borrowed: &'a B,
    consted: [u32; L],
    my_trait: V,
}
