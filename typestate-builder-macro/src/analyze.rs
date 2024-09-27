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

use std::collections::{HashMap, HashSet};

use petgraph::{graph::NodeIndex, visit::Dfs, Graph};
use proc_macro_error::emit_call_site_warning;
use syn::{Expr, GenericArgument, Ident, Lifetime, PathArguments, Type};

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
    let dfs_fields = map.get("Field0").map(|f| Dfs::new(&graph, *f));
    if let Some(mut dfs_fields) = dfs_fields {
        while let Some(ix_field) = dfs_fields.next(&graph) {
            let Some(StructElement::Field(field)) = graph.node_weight_mut(ix_field) else {
                panic!("Only Field is accepted.");
            };

            // List some field assets recursively.
            field.list();
            emit_call_site_warning!(format!(
                "{:#?}{:#?}{:#?}",
                field.idents, field.lifetimes, field.const_params
            ));

            // Search for field assets in the main generics and match.
            let dfs_generics = map.get("Generic0").map(|f| Dfs::new(&graph, *f));
            if let Some(mut dfs_generics) = dfs_generics {
                while let Some(ix_generic) = dfs_generics.next(&graph) {
                    let Some(StructElement::Generic(generic)) = graph.node_weight_mut(ix_generic)
                    else {
                        panic!("Only Generic is accepted.")
                    };
                }
            }

            // Search for field assets in where predicates.
        }
    }
    graph
}
