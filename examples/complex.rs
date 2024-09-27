#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

pub(crate) trait MyTrait {}

#[derive(TypestateBuilder)]
pub(crate) struct MultiBoundGeneric<'a, T, const L: usize, U, V>
where
    T: Clone + Default + std::fmt::Debug,
    U: Into<String> + Copy,
    V: MyTrait,
{
    item1: T,
    item2: U,
    data: Vec<T>,
    borrowed: &'a str,
    consted: [u32; L],
    my_trait: V,
}

fn main() {}
