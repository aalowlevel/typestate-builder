#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

pub(crate) trait MyTrait {}

#[derive(TypestateBuilder)]
pub(crate) struct MultiBoundGeneric<'a> {
    name: &'a str,
    age: u32,
    description: &'a str,
}

fn main() {}
