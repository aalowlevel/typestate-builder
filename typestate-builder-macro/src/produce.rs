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

use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::TokenStream as TokenStream2;

use crate::graph::StructGraph;

mod builder_states;

pub struct Produce {
    pub graph: StructGraph,
    pub map: IndexMap<String, NodeIndex>,
    pub res: Vec<TokenStream2>,
}

pub fn run(graph: StructGraph, map: IndexMap<String, NodeIndex>) -> Produce {
    let mut res = Vec::new();
    if let Some(builder_states) = builder_states::run(&graph, &map) {
        res.extend(builder_states);
    }
    Produce { graph, map, res }
}

struct Builder {}
struct BuilderNewImpl {}
struct BuilderImpl {}
struct BuilderBuildImpl {}
