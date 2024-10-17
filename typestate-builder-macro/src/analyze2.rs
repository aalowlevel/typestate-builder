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
use quote::format_ident;

use crate::{
    graph::{
        traverse, StructElement, StructGraph, StructRelation, BUILDER_IDENT, FIELD_START_P, IDENT,
    },
    helper::{to_titlecase, IX_IDENT_MSG, NODE_FIELD_MSG, NODE_IDENT_MSG},
};

pub fn run(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    create_builder(graph, map);
    create_builder_states(graph, map);
}

fn create_builder(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    /* ✅ #TD10362896 Create builder. */
    let Some(ix) = map.get(IDENT) else {
        panic!("{}", IX_IDENT_MSG);
    };
    let StructElement::Ident(ident) = &graph[*ix] else {
        panic!("{}", NODE_IDENT_MSG);
    };
    let ident = format_ident!("{}Builder", ident);
    let ix_builder = graph.add_node(StructElement::BuilderIdent(ident));
    map.insert(BUILDER_IDENT.to_string(), ix_builder);

    if let Some(ix) = map.get(FIELD_START_P) {
        /* ✅ #TD92175615 Traverse on field train. */
        let action = |graph: &StructGraph, _edge, field_node| {
            let StructElement::Field(field) = &graph[field_node] else {
                panic!("{}", NODE_FIELD_MSG);
            };
            let ident_str = field
                .syn
                .ident
                .as_ref()
                .map(|f| f.to_string())
                .unwrap_or_else(|| format!("field{}", field.nth));
            let ident = syn::Ident::new(&ident_str, Span::call_site());
            let ident_tc_str = to_titlecase(&ident_str);
            let ident_tc = syn::Ident::new(&ident_tc_str, Span::call_site());
            (ident, ident_tc)
        };
        let builder_data = traverse(
            graph,
            Some(&[&StructRelation::FieldTrain]),
            *ix,
            true,
            action,
        );

        /* ✅ #TD52440080 Create builder fields, generics and their relations. */
        let mut predecessor_ident = None;
        let mut predecessor_ident_tc = None;
        for (i, (ident, ident_tc)) in builder_data.into_iter().enumerate() {
            /* ✅ #TD72341053 Field */
            let successor = graph.add_node(StructElement::BuilderField(ident));
            if let Some(predecessor) = predecessor_ident.take() {
                graph.add_edge(predecessor, successor, StructRelation::BuilderFieldTrain);
            }
            predecessor_ident.get_or_insert(successor);
            let node_string = stringify!(BuilderField);
            let key = format!("{}{}", node_string, i);
            map.insert(key, successor);

            /* ✅ #TD45099623 Generic */
            let successor = graph.add_node(StructElement::BuilderGeneric(ident_tc));
            if let Some(predecessor) = predecessor_ident_tc.take() {
                graph.add_edge(predecessor, successor, StructRelation::BuilderGenericTrain);
            }
            predecessor_ident_tc.get_or_insert(successor);
            let node_string = stringify!(BuilderGeneric);
            let key = format!("{}{}", node_string, i);
            map.insert(key, successor);
        }
    }
}

fn create_builder_states(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {}
