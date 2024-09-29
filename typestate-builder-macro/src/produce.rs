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

use crate::graph::{traverse, StructElement, StructGraph, StructRelation, FIELD_START_P};

pub fn run(
    graph: StructGraph,
    map: IndexMap<String, NodeIndex>,
) -> (StructGraph, IndexMap<String, NodeIndex>) {
    let (graph, map) = BuilderStatePair::run(graph, map);
    (graph, map)
}

#[derive(Debug)]
struct BuilderStatePair<'a> {
    main_ident: &'a Ident,
    ident: Cow<'a, Ident>,
}
impl<'a> BuilderStatePair<'a> {
    fn run(
        graph: StructGraph,
        map: IndexMap<String, NodeIndex>,
    ) -> (StructGraph, IndexMap<String, NodeIndex>) {
        let action = |graph: &StructGraph, _edge, node| {
            let StructElement::Field(field) = &graph[node] else {
                return;
            };
            let Some(StructElement::Ident(main_ident)) = map.get("Ident").map(|f| &graph[*f])
            else {
                return;
            };

            let ident = if let Some(ident) = &field.syn.ident {
                Cow::Borrowed(ident)
            } else {
                Cow::Owned(Ident::new(
                    &format!("field{}", field.nth),
                    Span::call_site(),
                ))
            };

            let builder_state_pair = BuilderStatePair { main_ident, ident };
        };

        if let Some(start) = map.get(FIELD_START_P) {
            traverse(
                &graph,
                Some(&[&StructRelation::FieldTrain]),
                *start,
                true,
                action,
            );
        }
        (graph, map)
    }
}
struct Builder {}
struct BuilderNewImpl {}
struct BuilderImpl {}
struct BuilderBuildImpl {}
