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

#![warn(missing_docs)]

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

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, FieldsNamed, FieldsUnnamed};

/// Struct to hold the generated output of the `TypestateBuilder` macro.
struct TypestateBuilderOutPut {
    /// A collection of state structs used to represent the typestate of each field.
    state_structs: Vec<TokenStream2>,
    /// The builder struct itself, which is used to construct the final type.
    builder_struct: TokenStream2,
    /// A collection of methods for setting the fields of the builder in the correct order.
    builder_methods: Vec<TokenStream2>,
    /// The final build method that assembles the struct after all required fields are set.
    build_method: TokenStream2,
}

/// The `TypestateBuilder` derive macro generates builder pattern code based on the
/// typestate pattern. It provides compile-time guarantees that all necessary fields
/// are initialized before building the final struct.
///
/// # Panics
/// This macro will panic if applied to a non-struct type (such as an enum or union).
#[proc_macro_derive(TypestateBuilder)]
pub fn typestate_builder_derive(input: TokenStream) -> TokenStream {
    // Parse the input token stream into a `DeriveInput` structure.
    let input = parse_macro_input!(input as DeriveInput);

    // Match the type of struct and generate the appropriate builder code.
    let TypestateBuilderOutPut {
        state_structs,
        builder_struct,
        builder_methods,
        build_method,
    } = match &input.data {
        // Handle named field structs.
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => generate_named_struct_code(&input, fields),
            // Handle tuple structs (structs with unnamed fields).
            Fields::Unnamed(fields) => generate_tuple_struct_code(&input, fields),
            // Handle unit structs (structs with no fields).
            Fields::Unit => generate_unit_struct_code(&input),
        },
        // Panic if applied to anything other than a struct.
        _ => panic!("TypestateBuilder only supports structs"),
    };

    // Combine the generated code into a final token stream.
    let output = quote! {
        #(#state_structs)*

        #builder_struct

        #(#builder_methods)*

        #build_method
    };

    // Return the generated code.
    output.into()
}

/// Generates the builder code for structs with named fields.
///
/// # Arguments
///
/// * `input` - The parsed `DeriveInput` containing metadata about the struct.
/// * `fields` - The named fields of the struct.
///
/// # Returns
///
/// A `TypestateBuilderOutPut` struct containing the generated state structs,
/// builder struct, builder methods, and the build method for the named struct.
fn generate_named_struct_code(input: &DeriveInput, fields: &FieldsNamed) -> TypestateBuilderOutPut {
    todo!()
}

/// Generates the builder code for tuple structs (i.e., structs with unnamed fields).
///
/// # Arguments
///
/// * `input` - The parsed `DeriveInput` containing metadata about the struct.
/// * `fields` - The unnamed fields of the struct.
///
/// # Returns
///
/// A `TypestateBuilderOutPut` struct containing the generated state structs,
/// builder struct, builder methods, and the build method for the tuple struct.
fn generate_tuple_struct_code(
    input: &DeriveInput,
    fields: &FieldsUnnamed,
) -> TypestateBuilderOutPut {
    todo!()
}

/// Generates the builder code for unit structs (i.e., structs without fields).
///
/// # Arguments
///
/// * `input` - The parsed `DeriveInput` containing metadata about the struct.
///
/// # Returns
///
/// A `TypestateBuilderOutPut` struct containing the generated state structs,
/// builder struct, builder methods, and the build method for the unit struct.
fn generate_unit_struct_code(input: &DeriveInput) -> TypestateBuilderOutPut {
    todo!()
}
