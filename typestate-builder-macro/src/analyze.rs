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

use std::collections::HashMap;

use petgraph::{graph::NodeIndex, visit::Dfs, Graph};

use crate::{
    graph::{StructElement, StructRelation},
    write_graph_to_file,
};

pub fn run(graph: Graph<StructElement, StructRelation>, map: HashMap<String, NodeIndex>) {
    let graph = bind_field_elements(graph, &map);
    write_graph_to_file(&graph, "example.dot").unwrap();
}

fn bind_field_elements(
    mut graph: Graph<StructElement, StructRelation>,
    map: &HashMap<String, NodeIndex>,
) -> Graph<StructElement, StructRelation> {
    if let Some(mut dfs) = map.get("Field0").map(|start| Dfs::new(&graph, *start)) {
        // Traversal on field train.
        while let Some(node_field) = dfs.next(&graph) {
            list_field_assets(&mut graph, node_field);
            traversal_in_generics(&mut graph, node_field, map);
        }
    }
    graph
}

const ONLY_FIELD_MSG: &str = "Only Field is accepted.";
const ONLY_GENERIC_MSG: &str = "Only Generic is accepted.";
fn list_field_assets(graph: &mut Graph<StructElement, StructRelation>, node_field: NodeIndex) {
    let StructElement::Field(field) = &mut graph[node_field] else {
        panic!("{}", ONLY_FIELD_MSG);
    };
    field.list();
}
fn traversal_in_generics(
    graph: &mut Graph<StructElement, StructRelation>,
    node_field: NodeIndex,
    map: &HashMap<String, NodeIndex>,
) {
    if let Some(mut dfs) = map.get("Generic0").map(|start| Dfs::new(&*graph, *start)) {
        // Traversal on generic train.
        while let Some(node_generic) = dfs.next(&*graph) {
            search_in_generics(graph, node_field, node_generic);
        }
    }
}
/** Checks whether any element in the field is defined in the generics. If it is defined, establishes a connection. */
fn search_in_generics(
    graph: &mut Graph<StructElement, StructRelation>,
    node_field: NodeIndex,
    node_generic: NodeIndex,
) {
    let StructElement::Generic(generic) = &graph[node_generic] else {
        panic!("{}", ONLY_GENERIC_MSG);
    };
    let generic_ident = match generic {
        syn::GenericParam::Lifetime(lifetime_param) => &lifetime_param.lifetime.ident,
        syn::GenericParam::Type(type_param) => &type_param.ident,
        syn::GenericParam::Const(const_param) => &const_param.ident,
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
        graph.add_edge(node_field, node_generic, StructRelation::FieldGenerics);
    }
}
