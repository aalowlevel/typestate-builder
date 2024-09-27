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

//! This crate provides the `TypestateBuilder` derive macro for generating a
//! typestate-pattern builder for structs.
//!
//! # Typestate Pattern
//! The typestate pattern helps ensure that a struct is initialized in steps,
//! enforcing that certain fields must be set before others. This prevents
//! the creation of incomplete structs and provides compile-time safety for
//! field initialization.
//!
//! Example usage:
//!
//! ```rust
//! #[derive(TypestateBuilder)]
//! struct Person {
//!     name: String,
//!     age: u32,
//!     email: Option<String>,
//! }
//!
//! let person = Person::builder()
//!     .name("Alice".to_string())
//!     .age(30)
//!     .email(Some("alice@example.com".to_string()))
//!     .build();
//! ```

#![warn(missing_docs)]
#![allow(unused)]

mod analyze;
mod parse;
mod produce;

use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::proc_macro_error;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, GenericParam,
    Generics, Ident, Lifetime, Type, WhereClause, WherePredicate,
};

/// The `TypestateBuilder` derive macro generates builder pattern code based on the
/// typestate pattern. It provides compile-time guarantees that all necessary fields
/// are initialized before building the final struct.
///
/// # Panics
/// This macro will panic if applied to a non-struct type (such as an enum or union).
#[proc_macro_derive(TypestateBuilder)]
#[proc_macro_error]
pub fn typestate_builder_derive(input: TokenStream) -> TokenStream {
    // Parse the input token stream into a `DeriveInput` structure.
    let input = parse_macro_input!(input as DeriveInput);

    // Firstly, we need to parse input data.
    parse::init(input);

    // Match the type of struct and generate the appropriate builder code.
    // let TypestateBuilderOutPut {
    //     state_structs,
    //     builder_struct,
    //     builder_method,
    //     builder_constructor_methods,
    //     build_method,
    // } = match &input.data {
    //     // Handle named field structs.
    //     Data::Struct(data) => match &data.fields {
    //         Fields::Named(fields) => generate_named_struct_code(&input, fields),
    //         // Handle tuple structs (structs with unnamed fields).
    //         Fields::Unnamed(fields) => generate_tuple_struct_code(&input, fields),
    //         // Handle unit structs (structs with no fields).
    //         Fields::Unit => generate_unit_struct_code(&input),
    //     },
    //     // Panic if applied to anything other than a struct.
    //     _ => panic!("TypestateBuilder only supports structs"),
    // };

    // // Combine the generated code into a final token stream.
    // let output = quote! {
    //     #(#state_structs)*
    //     #builder_struct
    //     #builder_method
    //     #(#builder_constructor_methods)*
    //     #build_method
    // };

    // // Return the generated code.
    // output.into()
    quote! {}.into()
}
