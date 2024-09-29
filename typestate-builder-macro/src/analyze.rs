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
use petgraph::{graph::NodeIndex, visit::Dfs};
use proc_macro_error::emit_call_site_warning;
use syn::{GenericParam, WherePredicate};

use crate::{
    graph::{traverse_by_edge_mut, StructElement, StructGraph, StructRelation},
    helper::extract_ident,
};

pub fn run(
    graph: StructGraph,
    map: IndexMap<String, NodeIndex>,
) -> (StructGraph, IndexMap<String, NodeIndex>) {
    let graph = bind_field_elements(graph, &map);
    (graph, map)
}

fn bind_field_elements(mut graph: StructGraph, map: &IndexMap<String, NodeIndex>) -> StructGraph {
    if let Some(start) = map.get("Field0") {
        let action = |graph: &mut StructGraph, node, _edge| {
            list_field_assets(graph, node);
            // traversal_in_generics(graph, node, map);
            // traversal_in_where_clause(graph, node, map);
        };
        let visited = traverse_by_edge_mut(&mut graph, &StructRelation::FieldTrain, *start, action);

        let visited = visited
            .into_iter()
            .filter_map(|f| {
                let node = &graph[f];
                if let StructElement::Field(field) = node {
                    if let Some(ident) = &field.syn.ident {
                        return Some(ident.to_string());
                    }
                }
                None
            })
            .collect::<Vec<_>>();
        emit_call_site_warning!(visited.join(", "));
    }
    graph
}

const ONLY_FIELD_MSG: &str = "Only Field is accepted.";
const ONLY_GENERIC_MSG: &str = "Only Generic is accepted.";
const ONLY_WP_MSG: &str = "Only Where Predicate is accepted.";
fn list_field_assets(graph: &mut StructGraph, node_field: NodeIndex) {
    let StructElement::Field(field) = &mut graph[node_field] else {
        panic!("{}", ONLY_FIELD_MSG);
    };
    field.list();
}
fn traversal_in_generics(
    graph: &mut StructGraph,
    node_field: NodeIndex,
    map: &IndexMap<String, NodeIndex>,
) {
    if let Some(mut dfs) = map.get("Generic0").map(|start| Dfs::new(&*graph, *start)) {
        // Traversal on generic train.
        while let Some(node_generic) = dfs.next(&*graph) {
            search_in_generics(graph, node_field, node_generic);
        }
    }
}
/** Checks whether any element in the field is defined in the generics. If it is defined, establishes a connection. */
fn search_in_generics(graph: &mut StructGraph, node_field: NodeIndex, node_generic: NodeIndex) {
    let StructElement::Generic(generic) = &graph[node_generic] else {
        panic!("{}", ONLY_GENERIC_MSG);
    };
    let generic_ident = match generic {
        GenericParam::Lifetime(lifetime_param) => &lifetime_param.lifetime.ident,
        GenericParam::Type(type_param) => &type_param.ident,
        GenericParam::Const(const_param) => &const_param.ident,
    };
    let StructElement::Field(field) = &graph[node_field] else {
        panic!("{}", ONLY_FIELD_MSG);
    };
    let found = field
        .idents
        .iter()
        .any(|type_ident| type_ident == generic_ident)
        || field
            .lifetimes
            .iter()
            .any(|lifetime| &lifetime.ident == generic_ident)
        || field
            .const_params
            .iter()
            .any(|const_param_ident| const_param_ident == generic_ident);
    if found {
        graph.add_edge(
            node_field,
            node_generic,
            StructRelation::FieldGenericsInMain,
        );
    }
}
fn traversal_in_where_clause(
    graph: &mut StructGraph,
    node_field: NodeIndex,
    map: &IndexMap<String, NodeIndex>,
) {
    if let Some(mut dfs) = map
        .get("WherePredicate0")
        .map(|start| Dfs::new(&*graph, *start))
    {
        // Traversal on where predicate train.
        while let Some(node_wp) = dfs.next(&*graph) {
            search_in_wp(graph, node_field, node_wp);
        }
    }
}
/** Checks whether any element in the field is defined in where clause of the generics. If it is defined, establishes a connection. */
fn search_in_wp(graph: &mut StructGraph, node_field: NodeIndex, node_wp: NodeIndex) {
    let StructElement::WherePredicate(wp) = &graph[node_wp] else {
        panic!("{}", ONLY_WP_MSG);
    };
    let wp_ident = match wp {
        WherePredicate::Lifetime(predicate_lifetime) => Some(&predicate_lifetime.lifetime.ident),
        WherePredicate::Type(predicate_type) => extract_ident(&predicate_type.bounded_ty),
        _ => None,
    };
    let StructElement::Field(field) = &graph[node_field] else {
        panic!("{}", ONLY_FIELD_MSG);
    };
    let found = field
        .idents
        .iter()
        .any(|type_ident| Some(type_ident) == wp_ident)
        || field
            .lifetimes
            .iter()
            .any(|lifetime| Some(&lifetime.ident) == wp_ident)
        || field
            .const_params
            .iter()
            .any(|const_param_ident| Some(const_param_ident) == wp_ident);
    if found {
        graph.add_edge(
            node_field,
            node_wp,
            StructRelation::FieldGenericsInWhereClause,
        );
    }
}
