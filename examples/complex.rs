#![allow(dead_code)]

use std::marker::PhantomData;

use typestate_builder::TypestateBuilder;

fn main() {}

// #[derive(TypestateBuilder)]
struct AdvancedPhantomStruct<'a, T>
where
    T: 'a + Clone,
{
    reference: &'a T,
    marker: PhantomData<T>,
}
struct AdvancedPhantomStructReferenceAdded<'a, T>(&'a T)
where
    T: 'a + Clone;
struct AdvancedPhantomStructReferenceEmpty;
struct AdvancedPhantomStructMarkerAdded<'a, T>(PhantomData<T>)
where
    T: 'a + Clone;
struct AdvancedPhantomStructMarkerEmpty;
