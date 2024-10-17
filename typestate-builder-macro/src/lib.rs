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
//! This is the helper crate of [typestate-builder](https://docs.rs/typestate-builder/latest/typestate_builder/).

#![warn(missing_docs)]

mod analyze;
mod graph;
mod helper;
mod parse;
mod produce;

use graph::{StructElement, StructRelation};
use helper::write_graph_to_file;
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

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
    let (mut graph, map) = parse::run(input);
    analyze::run(&mut graph, &map);
    let produce::Produce { graph, res } = produce::run(graph, map);

    // Combine the generated code into a final token stream.
    let output = quote! {
        #(#res)*
    };

    write_graph_to_file(&graph, "example.dot").unwrap();
    output.into()
}
