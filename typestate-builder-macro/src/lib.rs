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
mod analyze2;
mod graph;
mod helper;
mod parse;
mod produce;

use graph::{StructElement, StructRelation};
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/**
The `TypestateBuilder` derive macro generates builder pattern code based on the
typestate pattern. It provides compile-time guarantees that all necessary fields
are initialized before building the final struct.
///
For more information, read [the document of the consumer crate](https://docs.rs/typestate-builder/latest/typestate_builder/).
///
# Panics
This macro will panic if applied to a non-struct type (such as an enum or union).
# Configuration Options

The macro supports configuration through attributes:

```rust
#[derive(TypestateBuilder)]
#[typestate_builder(
    builder_type = "PersonFactory",    // Custom builder type name
    builder_method = "create"          // Custom builder method name
)]
struct Person {
    name: String,
    age: u32,
}
```

## Available Attributes
* `builder_type`: Customize the generated builder struct name. Default: `{OriginalType}Builder`. Value must be a valid type name and is automatically converted to title case.
* `builder_method`: Customize the method name that creates a new builder. Default: `builder`. Value must be a valid method name and is automatically converted to lowercase.

# Field Configuration Options
*/
#[proc_macro_derive(TypestateBuilder, attributes(typestate_builder))]
#[proc_macro_error]
pub fn typestate_builder_derive(input: TokenStream) -> TokenStream {
    // Parse the input token stream into a `DeriveInput` structure.
    let input = parse_macro_input!(input as DeriveInput);

    let filename = format!("{}.dot", input.ident);

    let (mut graph, mut map) = parse::run(input);
    analyze::run(&mut graph, &mut map);
    analyze2::run(&mut graph, &mut map);
    let res = produce::run(&graph, &map);

    // Combine the generated code into a final token stream.
    let output = quote! {
        #(#res)*
    };

    #[cfg(debug_assertions)]
    {
        helper::write_graph_to_file(&graph, &filename).unwrap();
    }
    output.into()
}
