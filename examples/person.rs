// Copyright (c) 2024 Andy Allison
//
// Licensed under either of
//
// * MIT license (LICENSE-MIT or http://opensource.org/licenses/MIT)
// * Apache License, Version 2.0 (LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)
//
// at your option.
//
// Unless you explicitly state otherwise, any contribution intentionally submitted
// for inclusion in the work by you, as defined in the Apache-2.0 license, shall
// be dual licensed as above, without any additional terms or conditions.

#![allow(dead_code)]

use typestate_builder::TypestateBuilder;

#[derive(TypestateBuilder)]
pub struct Person {
    name: String,
    age: u32,
    email: Option<String>,
}

fn main() {
    // let person = Person::builder()
    //     .name("Alice Johnson".to_string())
    //     .age(30)
    //     .email(None)
    //     .build();

    // println!("{:?}", person);
}
