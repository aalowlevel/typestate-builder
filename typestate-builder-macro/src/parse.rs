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
    let parse_main_options = ParseMainOptions::new(input.attrs);

    /* ✅ #TD24693499 Describe builder type ident. */
    {
        let ident = if let Some(builder_type) = &parse_main_options.builder_type {
            format_ident!("{}", builder_type)
        } else {
            format_ident!("{}Builder", input.ident)
        };
        let ix = graph.add_node(StructElement::BuilderIdent(Rc::new(ident)));
        map.insert(mapkey::uniq::BUILDER_IDENT.to_string(), ix);
    }

    /* ✅ #TD28740689 Describe builder method name. */
    {
        let ident = if let Some(builder_method) = &parse_main_options.builder_method {
            format_ident!("{}", builder_method)
        } else {
            format_ident!("builder")
        };
        let ix = graph.add_node(StructElement::MethodBuilderIdent(Rc::new(ident)));
        map.insert(mapkey::uniq::METHOD_BUILDER_IDENT.to_string(), ix);
    }

    // Beginning
    {
        let ix = graph.add_node(StructElement::Visibility(input.vis));
        map.insert(mapkey::uniq::VIS.to_string(), ix);
        let ix = graph.add_node(StructElement::Ident(input.ident));
        map.insert(mapkey::uniq::IDENT.to_string(), ix);
    }

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
                .map(map_syn_field)
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
                .map(map_syn_field)
                .collect::<Vec<_>>();
            add_from_list!(graph, map, fields, Field, FieldTrain);
        }
        Fields::Unit => {
            panic!("Since it does not need to be built, Unit Struct Type is not supported.")
        }
    }

    (graph, map)
}

fn map_syn_field((nth, syn): (usize, syn::Field)) -> Field {
    Field {
        nth,
        syn,
        types: IndexSet::new(),
        lifetimes: IndexSet::new(),
        const_params: IndexSet::new(),
        default: false,
    }
}

#[derive(Default)]
struct ParseMainOptions {
    builder_type: Option<String>,
    builder_method: Option<String>,
}

/// Parser for the entire attribute content
struct AttributeArgs {
    attrs: syn::punctuated::Punctuated<syn::Meta, syn::Token![,]>,
}

impl syn::parse::Parse for AttributeArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: syn::punctuated::Punctuated::parse_terminated(input)?,
        })
    }
}

impl ParseMainOptions {
    fn new(attrs: Vec<syn::Attribute>) -> Self {
        let mut parse_main_args = ParseMainOptions::default();

        // Capture `typestate_builder` attributes specified in the struct
        for attr in attrs {
            if !attr.path().is_ident("typestate_builder") {
                continue;
            }
            let syn::Meta::List(meta_list) = attr.meta else {
                panic!("typestate_builder attribute must be a list.")
            };

            if let Ok(attrs) = syn::parse2::<AttributeArgs>(meta_list.tokens) {
                for meta in attrs.attrs {
                    if meta.path().is_ident("builder_type") {
                        let lit_str = Self::value_litstr_validation(meta);
                        let titlecase = helper::string::to_titlecase(&lit_str.value());
                        parse_main_args.builder_type = Some(titlecase);
                    } else if meta.path().is_ident("builder_method") {
                        let lit_str = Self::value_litstr_validation(meta);
                        parse_main_args.builder_method = Some(lit_str.value().to_lowercase());
                    }
                }
            }
        }

        parse_main_args
    }

    fn value_litstr_validation(meta: syn::Meta) -> syn::LitStr {
        let syn::Meta::NameValue(syn::MetaNameValue {
            value:
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }),
            ..
        }) = meta
        else {
            panic!("This attribute must be key = \"value\" syntax.");
        };
        lit_str
    }
}
