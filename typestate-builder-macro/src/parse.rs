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

use std::rc::Rc;

use crate::{
    graph::{mapkey, Field, GenericParam, StructGraph, StructType, WherePredicate},
    helper, StructElement, StructRelation,
};

use indexmap::{IndexMap, IndexSet};
use petgraph::{graph::NodeIndex, Graph};
use quote::format_ident;
use syn::{Data, DeriveInput, Fields};

macro_rules! add_from_list {
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

pub fn run(input: DeriveInput) -> (StructGraph, IndexMap<String, NodeIndex>) {
    let Data::Struct(data_struct) = input.data else {
        panic!("TypestateBuilder only supports structs");
    };

    let mut graph = Graph::<StructElement, StructRelation>::new();
    let mut map = IndexMap::new();

    /* ✅ #TD80531813 Set options */
    let parse_main_options = parse_main_options(&input.attrs);
    let ident = if let Some(custom_builder_name) = &parse_main_options.custom_builder_name {
        format_ident!("{}", custom_builder_name)
    } else {
        format_ident!("{}Builder", input.ident)
    };
    let ix = graph.add_node(StructElement::BuilderIdent(Rc::new(ident)));
    map.insert(mapkey::uniq::BUILDER_IDENT.to_string(), ix);

    // Beginning
    {
        let ix = graph.add_node(StructElement::Visibility(input.vis));
        map.insert(mapkey::uniq::VIS.to_string(), ix);
        let ix = graph.add_node(StructElement::Ident(input.ident));
        map.insert(mapkey::uniq::IDENT.to_string(), ix);
    }

    add_from_list!(graph, map, input.attrs, Attribute, AttributeTrain);
    let generics = input
        .generics
        .params
        .into_iter()
        .enumerate()
        .map(|(nth, syn)| GenericParam {
            nth,
            syn: Rc::new(syn),
        })
        .collect::<Vec<_>>();
    add_from_list!(graph, map, generics, Generic, GenericTrain);

    if let Some(where_clause) = input.generics.where_clause {
        let where_clauses = where_clause
            .predicates
            .into_iter()
            .enumerate()
            .map(|(nth, syn)| WherePredicate {
                nth,
                syn: Rc::new(syn),
                left_bound_lifetimes: None,
                left_bounded_type: None,
                left_bounded_lifetime: None,
                right_bounding_types: None,
                right_bounding_lifetimes: None,
                right_bounding_phantoms: None,
            })
            .collect::<Vec<_>>();
        add_from_list!(
            graph,
            map,
            where_clauses,
            WherePredicate,
            WherePredicateTrain
        );
    }

    match data_struct.fields {
        Fields::Named(fields_named) => {
            let ix = graph.add_node(StructElement::Type(StructType::Named));
            map.insert(mapkey::uniq::TYPE.to_string(), ix);

            let fields = fields_named
                .named
                .into_iter()
                .enumerate()
                .map(|(nth, syn)| Field {
                    nth,
                    syn,
                    types: IndexSet::new(),
                    lifetimes: IndexSet::new(),
                    const_params: IndexSet::new(),
                })
                .collect::<Vec<_>>();
            add_from_list!(graph, map, fields, Field, FieldTrain);
        }
        Fields::Unnamed(fields_unnamed) => {
            let ix = graph.add_node(StructElement::Type(StructType::Unnamed));
            map.insert(mapkey::uniq::TYPE.to_string(), ix);

            let fields = fields_unnamed
                .unnamed
                .into_iter()
                .enumerate()
                .map(|(nth, syn)| Field {
                    nth,
                    syn,
                    types: IndexSet::new(),
                    lifetimes: IndexSet::new(),
                    const_params: IndexSet::new(),
                })
                .collect::<Vec<_>>();
            add_from_list!(graph, map, fields, Field, FieldTrain);
        }
        Fields::Unit => {
            panic!("Since it does not need to be built, Unit Struct Type is not supported.")
        }
    }

    (graph, map)
}

struct ParseMainOptions {
    custom_builder_name: Option<String>,
}

fn parse_main_options(attrs: &[syn::Attribute]) -> ParseMainOptions {
    let mut parse_main_args = ParseMainOptions {
        custom_builder_name: None,
    };

    // Capture `ts_builder` attributes specified in the struct
    for attr in attrs.iter() {
        if !attr.path().is_ident("ts_builder") {
            continue;
        }

        if let Err(e) = attr.parse_nested_meta(|parse_nested_meta| {
            if parse_nested_meta.path.is_ident("custom_builder_name") {
                let Ok(value) = parse_nested_meta.value() else {
                    panic!("This attribute must be key = \"value\" syntax.");
                };
                let Ok(lit_str) = value.parse::<syn::LitStr>() else {
                    panic!("Inparsable attribute.");
                };
                let titlecase = helper::string::to_titlecase(&lit_str.value());
                parse_main_args.custom_builder_name = Some(titlecase);
                return Ok(());
            }

            panic!("Invalid attributes. Available names:\n- custom_builder_name")
        }) {
            panic!("{}", e);
        }
    }

    parse_main_args
}
