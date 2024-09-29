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
use proc_macro2::Span;
use syn::Ident;

use crate::graph::{traverse, StructElement, StructGraph, StructRelation, FIELD_START_P};

pub fn run(
    graph: StructGraph,
    map: IndexMap<String, NodeIndex>,
) -> (StructGraph, IndexMap<String, NodeIndex>) {
    let (graph, map, builder_states) = BuilderStates::run(graph, map);
    proc_macro_error::emit_call_site_warning!(format!("{:?}", builder_states));
    (graph, map)
}

#[derive(Debug)]
struct BuilderStates(IndexMap<NodeIndex, BuilderStatePair>);

impl BuilderStates {
    fn run(
        graph: StructGraph,
        map: IndexMap<String, NodeIndex>,
    ) -> (StructGraph, IndexMap<String, NodeIndex>, Self) {
        let action = |graph: &StructGraph, _edge, node| -> BuilderStatePair {
            let StructElement::Field(field) = &graph[node] else {
                panic!("Node must be a field.");
            };
            let Some(StructElement::Ident(main_ident)) = map.get("Ident").map(|f| &graph[*f])
            else {
                panic!("Struct must have an ident.");
            };

            let ident = if let Some(ident) = &field.syn.ident {
                ident.clone()
            } else {
                Ident::new(&format!("field{}", field.nth), Span::call_site())
            };

            BuilderStatePair {
                main_ident: main_ident.clone(),
                ident,
            }
        };

        if let Some(start) = map.get(FIELD_START_P) {
            let visited = traverse(
                &graph,
                Some(&[&StructRelation::FieldTrain]),
                *start,
                true,
                action,
            );
            (graph, map, Self(visited))
        } else {
            (graph, map, Self(IndexMap::new()))
        }
    }
}

#[derive(Debug)]
struct BuilderStatePair {
    main_ident: Ident,
    ident: Ident,
}

struct Builder {}
struct BuilderNewImpl {}
struct BuilderImpl {}
struct BuilderBuildImpl {}
