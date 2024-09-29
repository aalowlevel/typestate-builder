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
use syn::{GenericParam, WherePredicate};

use crate::{
    graph::{
        traverse_mut, StructElement, StructGraph, StructRelation, FIELD_START_P, GENERICS_START_P,
        WHERE_PREDICATE_START_P,
    },
    helper::extract_ident,
};

pub fn run(graph: &mut StructGraph, map: &IndexMap<String, NodeIndex>) {
    bind_field_elements(graph, map);
}

fn bind_field_elements(graph: &mut StructGraph, map: &IndexMap<String, NodeIndex>) {
    if let Some(start) = map.get(FIELD_START_P) {
        let action = |graph: &mut StructGraph, _edge, node_field| {
            list_field_assets(graph, node_field);
            traversal_in_generics(graph, node_field, map);
            traversal_in_where_clause(graph, node_field, map);
        };
        traverse_mut(
            graph,
            Some(&[&StructRelation::FieldTrain]),
            *start,
            true,
            action,
        );
    }
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
    if let Some(start) = map.get(GENERICS_START_P) {
        let action = |graph: &mut StructGraph, _edge, node_generic| {
            search_in_generics(graph, node_field, node_generic);
        };
        traverse_mut(
            graph,
            Some(&[&StructRelation::GenericTrain]),
            *start,
            true,
            action,
        );
    }
}
/** Checks whether any element in the field is defined in the generics. If it is defined, establishes a connection. */
fn search_in_generics(graph: &mut StructGraph, node_field: NodeIndex, node_generic: NodeIndex) {
    let StructElement::Generic(generic) = &graph[node_generic] else {
        panic!("{}", ONLY_GENERIC_MSG);
    };
    let generic_ident = match generic.syn.as_ref() {
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
    if let Some(start) = map.get(WHERE_PREDICATE_START_P) {
        let action = |graph: &mut StructGraph, _edge, node_wp| {
            search_in_wp(graph, node_field, node_wp);
        };

        traverse_mut(
            graph,
            Some(&[&StructRelation::WherePredicateTrain]),
            *start,
            true,
            action,
        );
    }
}
/** Checks whether any element in the field is defined in where clause of the generics. If it is defined, establishes a connection. */
fn search_in_wp(graph: &mut StructGraph, node_field: NodeIndex, node_wp: NodeIndex) {
    let StructElement::WherePredicate(wp) = &graph[node_wp] else {
        panic!("{}", ONLY_WP_MSG);
    };
    let wp_ident = match wp.syn.as_ref() {
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
