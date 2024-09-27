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

use fixedbitset::FixedBitSet;
use petgraph::{graph::NodeIndex, visit::Dfs, Graph};
use syn::GenericParam;

use crate::{
    graph::{Field, StructElement, StructRelation},
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
    if let Some(mut dfs_fields) = get_start_node(&graph, map, "Field0") {
        // Traverse on field train.
        while let Some(ix_field) = dfs_fields.next(&graph) {
            let field = get_node_as_field(&mut graph, ix_field);

            // List some field assets recursively.
            field.list();

            // Search for field assets in the main generics and match.
            if let Some(mut dfs_generics) = get_start_node(&graph, map, "Generic0") {
                // Traverse on generic train.
                while let Some(ix_generic) = dfs_generics.next(&graph) {
                    let generic = get_node_as_generic(&mut graph, ix_generic);
                }
            }

            // Search for field assets in where predicates.
        }
    }
    graph
}

fn get_start_node(
    graph: &Graph<StructElement, StructRelation>,
    map: &HashMap<String, NodeIndex>,
    key: &str,
) -> Option<Dfs<NodeIndex, FixedBitSet>> {
    map.get(key).map(|f| Dfs::new(graph, *f))
}

fn get_node_as_field(
    graph: &mut Graph<StructElement, StructRelation>,
    index: NodeIndex,
) -> &mut Field {
    match graph.node_weight_mut(index) {
        Some(StructElement::Field(field)) => field,
        _ => panic!("Only Field is accepted."),
    }
}

fn get_node_as_generic(
    graph: &mut Graph<StructElement, StructRelation>,
    index: NodeIndex,
) -> &mut GenericParam {
    match graph.node_weight_mut(index) {
        Some(StructElement::Generic(generic)) => generic,
        _ => panic!("Only Generic is accepted."),
    }
}
