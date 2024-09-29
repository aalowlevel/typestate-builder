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

use std::borrow::Cow;

use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::Span;
use syn::Ident;

use crate::graph::{traverse_by_edge, StructElement, StructGraph, StructRelation, FIELD_START_P};

pub fn run(
    graph: StructGraph,
    map: IndexMap<String, NodeIndex>,
) -> (StructGraph, IndexMap<String, NodeIndex>) {
    let (graph, map) = BuilderStatePair::run(graph, map);
    (graph, map)
}

struct BuilderStatePair<'a> {
    ident: Cow<'a, Ident>,
}
impl<'a> BuilderStatePair<'a> {
    fn run(
        graph: StructGraph,
        map: IndexMap<String, NodeIndex>,
    ) -> (StructGraph, IndexMap<String, NodeIndex>) {
        if let Some(start) = map.get(FIELD_START_P) {
            let action = |graph: &StructGraph, node, edge| {
                let StructElement::Field(field) = &graph[node] else {
                    return;
                };
                let builder_state_pair = BuilderStatePair::new(&field.syn);
            };
            let visited = traverse_by_edge(&graph, &StructRelation::FieldTrain, *start, action);
        }
        (graph, map)
    }

    fn new(field: &'a syn::Field) -> Self {
        let ident = if let Some(ident) = &field.ident {
            Cow::Borrowed(ident)
        } else {
            Cow::Owned(Ident::new("field", Span::call_site()))
        };
        Self { ident }
    }
}
struct Builder {}
struct BuilderNewImpl {}
struct BuilderImpl {}
struct BuilderBuildImpl {}
