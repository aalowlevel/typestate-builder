#![allow(dead_code)]

use std::marker::PhantomData;

use typestate_builder::TypestateBuilder;

fn main() {}

pub(crate) trait MyTrait {}

#[derive(TypestateBuilder)]
struct AdvancedPhantomStruct<'a, T>
where
    T: 'a + Clone,
{
    reference: &'a T,
    marker: PhantomData<T>,
}
