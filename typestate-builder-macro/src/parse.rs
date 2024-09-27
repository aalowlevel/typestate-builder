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

use crate::{graph::Field, StructElement, StructRelation};

use std::collections::{HashMap, HashSet};

use petgraph::{graph::NodeIndex, Graph};
use syn::{Data, DeriveInput, Fields};

macro_rules! add_from_syn_list {
    ($graph:expr, $map:expr, $list:expr, $node:ident, $edge:ident) => {{
        let mut predecessor = None;
        for (i, attr) in $list.into_iter().enumerate() {
            let successor = $graph.add_node(StructElement::$node(attr));
            if let Some(predecessor) = predecessor.take() {
                $graph.add_edge(predecessor, successor, StructRelation::$edge);
            }
            predecessor.get_or_insert(successor);
            let node_string = stringify!($node);
            let key = format!("{}{}", node_string, i);
            $map.insert(key, successor);
        }
    }};
}

pub fn run(
    input: DeriveInput,
) -> (
    Graph<StructElement, StructRelation>,
    HashMap<String, NodeIndex>,
) {
    let Data::Struct(data_struct) = input.data else {
        panic!("TypestateBuilder only supports structs");
    };

    let mut graph = Graph::<StructElement, StructRelation>::new();
    let mut map = HashMap::new();

    // Beginning
    {
        let ix = graph.add_node(StructElement::Visibility(input.vis));
        map.insert("Visibility".to_string(), ix);
        let ix = graph.add_node(StructElement::Ident(input.ident));
        map.insert("Ident".to_string(), ix);
    }

    add_from_syn_list!(graph, map, input.attrs, Attribute, AttributeTrain);
    add_from_syn_list!(graph, map, input.generics.params, Generic, GenericTrain);
    if let Some(where_clause) = input.generics.where_clause {
        add_from_syn_list!(
            graph,
            map,
            where_clause.predicates,
            WherePredicate,
            WherePredicateTrain
        );
    }

    match data_struct.fields {
        Fields::Named(fields_named) => {
            let fields = fields_named
                .named
                .into_iter()
                .map(|f| Field {
                    syn: f,
                    idents: HashSet::new(),
                    lifetimes: HashSet::new(),
                    const_params: HashSet::new(),
                })
                .collect::<Vec<_>>();
            add_from_syn_list!(graph, map, fields, Field, FieldTrain);
        }
        Fields::Unnamed(fields_unnamed) => {
            let fields = fields_unnamed
                .unnamed
                .into_iter()
                .map(|f| Field {
                    syn: f,
                    idents: HashSet::new(),
                    lifetimes: HashSet::new(),
                    const_params: HashSet::new(),
                })
                .collect::<Vec<_>>();
            add_from_syn_list!(graph, map, fields, Field, FieldTrain);
        }
        Fields::Unit => {}
    }

    (graph, map)
}
