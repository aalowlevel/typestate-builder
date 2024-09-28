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
        while let Some(node) = dfs.next(&graph) {
            list_field_assets(&mut graph, node);
            search_in_generics(&mut graph, node, map);
        }
    }
    graph
}

const ONLY_FIELD_MSG: &str = "Only Field is accepted.";
fn list_field_assets(graph: &mut Graph<StructElement, StructRelation>, node: NodeIndex) {
    let StructElement::Field(field) = &mut graph[node] else {
        panic!("{}", ONLY_FIELD_MSG);
    };
    field.list();
}
fn search_in_generics(
    graph: &mut Graph<StructElement, StructRelation>,
    node: NodeIndex,
    map: &HashMap<String, NodeIndex>,
) {
    let StructElement::Field(field) = &graph[node] else {
        panic!("{}", ONLY_FIELD_MSG);
    };

    if let Some(mut dfs) = map.get("Generic0").map(|start| Dfs::new(&*graph, *start)) {
        // Traversal on generic train.
        while let Some(node) = dfs.next(&*graph) {}
    }
}
