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

mod builder;
mod builder_build_impl;
mod builder_impl;
mod builder_new_impl;
mod builder_states;

use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::TokenStream as TokenStream2;

use crate::graph::StructGraph;

pub fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Vec<TokenStream2> {
    let mut res = Vec::new();
    if let Some(builder_states) = builder_states::run(&graph, &map) {
        res.extend(builder_states);
    }
    if let Some(builder) = builder::run(&graph, &map) {
        res.push(builder);
    }
    if let Some(builder_build_impl) = builder_build_impl::run(&graph, &map) {
        res.push(builder_build_impl);
    }
    res
}
