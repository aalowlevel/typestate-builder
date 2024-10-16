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

#![allow(dead_code)]

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

pub struct Produce {
    pub graph: StructGraph,
    pub map: IndexMap<String, NodeIndex>,
    pub res: Vec<TokenStream2>,
}

pub fn run(graph: StructGraph, map: IndexMap<String, NodeIndex>) -> Produce {
    let mut res = Vec::new();
    if let Some(builder_states) = BuilderStates::run(&graph, &map) {
        res.extend(builder_states);
    }
    Produce { graph, map, res }
}

struct BuilderStates;

impl BuilderStates {
    fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Option<Vec<TokenStream2>> {
        let action = |graph: &StructGraph, _edge, field_node| -> TokenStream2 {
            // All data to crate two-legged state structs.
            let BuilderStatePair {
                main_ident,
                ident,
                ty,
                field_to_main_lifetimes,
                field_to_main_consts,
                field_to_main_types,
                field_to_where_predicates,
            } = BuilderStatePair::new(graph, field_node, map);

            // Idents of pair.
            let ident_added = format_ident!("{}{}Added", main_ident, ident_to_titlecase(&ident));
            let ident_empty = format_ident!("{}{}Empty", main_ident, ident_to_titlecase(&ident));

            // Determining where predicates related to the field.
            let mut where_predicates = Vec::new();
            for field_to_where_predicate in field_to_where_predicates {
                let mut predicate = (*field_to_where_predicate.wp).clone();

                // Anomaly 1: Orphan wp right lifetimes. Add Higher-ranked trait bounds for them (ForLifetimes).
                if field_to_where_predicate
                    .right_lifetimes_in_generics
                    .iter()
                    .any(|p0| {
                        !field_to_main_lifetimes
                            .iter()
                            .any(|p1| match (p0.as_ref(), p1.as_ref()) {
                                (
                                    syn::GenericParam::Lifetime(lifetime_param0),
                                    syn::GenericParam::Lifetime(lifetime_param1),
                                ) => lifetime_param0.lifetime == lifetime_param1.lifetime,
                                (
                                    syn::GenericParam::Type(type_param0),
                                    syn::GenericParam::Type(type_param1),
                                ) => type_param0.ident == type_param1.ident,
                                _ => false,
                            })
                    })
                {
                    if let syn::WherePredicate::Type(pty) = &mut predicate {
                        if let Some(ptyb) = &mut pty.lifetimes {
                            let incoming_lts = field_to_where_predicate
                                .right_lifetimes_in_generics
                                .into_iter()
                                .map(|f| (*f).clone())
                                .collect::<Vec<_>>();
                            ptyb.lifetimes.extend(incoming_lts);
                        } else if !field_to_where_predicate
                            .right_lifetimes_in_generics
                            .is_empty()
                        {
                            let mut lts = syn::BoundLifetimes::default();
                            lts.lifetimes.extend(
                                field_to_where_predicate
                                    .right_lifetimes_in_generics
                                    .into_iter()
                                    .map(|f| (*f).clone())
                                    .collect::<Vec<_>>(),
                            );
                            pty.lifetimes = Some(lts);
                        }
                    }
                }

                where_predicates.push(predicate);
            }
            let where_clause = if where_predicates.is_empty() {
                quote! {}
            } else {
                quote! { where #(#where_predicates),* }
            };

            // Determining main generics related to the field.
            let mut generics = Vec::new();
            generics.extend(field_to_main_lifetimes);
            generics.extend(field_to_main_consts);
            generics.extend(field_to_main_types);
            let generics = if generics.is_empty() {
                quote! {}
            } else {
                quote! { < #(#generics),* > }
            };

            quote! {
                struct #ident_added #generics(#ty) #where_clause;
                struct #ident_empty;
            }
        };

        // Traverse on field train.
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

struct FieldToWherePredicate {
    wp: Rc<syn::WherePredicate>,
    right_lifetimes_in_generics: Vec<Rc<syn::GenericParam>>,
}

struct BuilderStatePair<'a> {
    main_ident: &'a Ident,
    ident: Cow<'a, Ident>,
    ty: &'a Type,
    field_to_main_lifetimes: Vec<Rc<syn::GenericParam>>,
    field_to_main_consts: Vec<Rc<syn::GenericParam>>,
    field_to_main_types: Vec<Rc<syn::GenericParam>>,
    field_to_where_predicates: Vec<FieldToWherePredicate>,
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
        let field_to_main_lifetimes = traverse(
            graph,
            Some(&[&StructRelation::FieldGenericInMainLifetime]),
            field_node,
            false,
            Self::traverse_field_to_main_generics,
        );
        let field_to_main_consts = traverse(
            graph,
            Some(&[&StructRelation::FieldGenericInMainConst]),
            field_node,
            false,
            Self::traverse_field_to_main_generics,
        );
        let field_to_main_types = traverse(
            graph,
            Some(&[&StructRelation::FieldGenericInMainType]),
            field_node,
            false,
            Self::traverse_field_to_main_generics,
        );
        let field_to_where_predicates = traverse(
            graph,
            Some(&[&StructRelation::FieldGenericInWhereClause]),
            field_node,
            false,
            Self::traverse_field_to_where_predicate,
        );
        Self {
            main_ident,
            ident,
            ty: &field.syn.ty,
            field_to_main_lifetimes,
            field_to_main_consts,
            field_to_main_types,
            field_to_where_predicates,
        }
    }

    fn traverse_field_to_main_generics(
        graph: &StructGraph,
        _edge: Option<EdgeIndex>,
        generic_node: NodeIndex,
    ) -> Rc<syn::GenericParam> {
        let StructElement::Generic(generic) = &graph[generic_node] else {
            panic!("Node must be a generic.");
        };
        Rc::clone(&generic.syn)
    }

    fn traverse_field_to_where_predicate(
        graph: &StructGraph,
        _edge: Option<EdgeIndex>,
        wp_node: NodeIndex,
    ) -> FieldToWherePredicate {
        let StructElement::WherePredicate(wp) = &graph[wp_node] else {
            panic!("Node must be a Where Predicate.");
        };
        let right_lifetimes_in_generics = traverse(
            graph,
            Some(&[&StructRelation::WPRightBoundingLifetimeInMain]),
            wp_node,
            false,
            Self::traverse_wp_to_main_generics,
        );
        FieldToWherePredicate {
            wp: Rc::clone(&wp.syn),
            right_lifetimes_in_generics,
        }
    }

    fn traverse_wp_to_main_generics(
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
