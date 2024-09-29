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

use std::borrow::Cow;
use std::rc::Rc;

use indexmap::IndexMap;
use petgraph::graph::EdgeIndex;
use petgraph::graph::NodeIndex;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::format_ident;
use quote::quote;
use syn::Ident;
use syn::Type;

use crate::graph::{traverse, StructElement, StructGraph, StructRelation, FIELD_START_P};
use crate::helper::ident_to_titlecase;

pub fn run(
    graph: StructGraph,
    map: IndexMap<String, NodeIndex>,
) -> (StructGraph, IndexMap<String, NodeIndex>, Vec<TokenStream2>) {
    let mut res = Vec::new();
    if let Some(builder_states) = BuilderStates::run(&graph, &map) {
        res.extend(builder_states.into_iter().map(|(_k, v)| v));
    }
    // proc_macro_error::emit_call_site_warning!(format!("{:#?}", builder_states));
    (graph, map, res)
}

struct BuilderStates;

impl BuilderStates {
    fn run(
        graph: &StructGraph,
        map: &IndexMap<String, NodeIndex>,
    ) -> Option<IndexMap<NodeIndex, TokenStream2>> {
        let action = |graph: &StructGraph, _edge, field_node| -> TokenStream2 {
            let BuilderStatePair {
                main_ident,
                ident,
                ty,
                to_main_generics,
            } = BuilderStatePair::new(graph, field_node, map);
            let ident_added = format_ident!("{}{}Added", main_ident, ident_to_titlecase(&ident));
            let ident_empty = format_ident!("{}{}Empty", main_ident, ident_to_titlecase(&ident));
            let to_main_generics = to_main_generics
                .into_iter()
                .map(|(_k, v)| v)
                .collect::<Vec<_>>();
            quote! {
                struct #ident_added< #(#to_main_generics),* >(#ty);
                struct #ident_empty;
            }
        };

        map.get(FIELD_START_P).map(|start| {
            traverse(
                graph,
                Some(&[&StructRelation::FieldTrain]),
                *start,
                true,
                action,
            )
        })
    }
}

struct BuilderStatePair<'a> {
    main_ident: &'a Ident,
    ident: Cow<'a, Ident>,
    ty: &'a Type,
    to_main_generics: IndexMap<NodeIndex, Rc<syn::GenericParam>>,
}

impl<'a> BuilderStatePair<'a> {
    fn new(
        graph: &'a StructGraph,
        field_node: NodeIndex,
        map: &'a IndexMap<String, NodeIndex>,
    ) -> Self {
        let StructElement::Field(field) = &graph[field_node] else {
            panic!("Node must be a field.");
        };
        let Some(StructElement::Ident(main_ident)) = map.get("Ident").map(|f| &graph[*f]) else {
            panic!("Struct must have an ident.");
        };
        let ident = if let Some(ident) = &field.syn.ident {
            Cow::Borrowed(ident)
        } else {
            Cow::Owned(Ident::new(
                &format!("field{}", field.nth),
                Span::call_site(),
            ))
        };
        let ty = &field.syn.ty;
        let to_main_generics = traverse(
            graph,
            Some(&[&StructRelation::FieldGenericsInMain]),
            field_node,
            false,
            Self::traverse_to_main_generics,
        );
        Self {
            main_ident,
            ident,
            ty,
            to_main_generics,
        }
    }

    fn traverse_to_main_generics(
        graph: &StructGraph,
        _edge: Option<EdgeIndex>,
        generic_node: NodeIndex,
    ) -> Rc<syn::GenericParam> {
        let StructElement::Generic(generic) = &graph[generic_node] else {
            panic!("Node must be a generic.");
        };
        Rc::clone(&generic.syn)
    }
}

struct Builder {}
struct BuilderNewImpl {}
struct BuilderImpl {}
struct BuilderBuildImpl {}
