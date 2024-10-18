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

use std::{borrow::Cow, rc::Rc};

use indexmap::IndexMap;
use petgraph::graph::{EdgeIndex, NodeIndex};
use proc_macro2::Span;
use quote::format_ident;

use crate::{
    graph::{
        mapkey, msg, traverse, traverse_mut, BuilderStateAdded, StructElement, StructGraph,
        StructRelation,
    },
    helper::to_titlecase,
};

pub fn run(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    create_builder(graph, map);
    create_builder_states(graph, map);
}

fn create_builder(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    /* ✅ #TD10362896 Create builder. */
    let Some(ix) = map.get(mapkey::uniq::IDENT) else {
        panic!("{}", msg::ix::IDENT);
    };
    let StructElement::Ident(ident) = &graph[*ix] else {
        panic!("{}", msg::node::IDENT);
    };
    let ident = format_ident!("{}Builder", ident);
    let ix_builder = graph.add_node(StructElement::BuilderIdent(Rc::new(ident)));
    map.insert(mapkey::uniq::BUILDER_IDENT.to_string(), ix_builder);

    if let Some(ix) = map.get(mapkey::startp::FIELD) {
        /* ✅ #TD92175615 Traverse on field train. */
        let action = |graph: &StructGraph, _edge, field_node| {
            let StructElement::Field(field) = &graph[field_node] else {
                panic!("{}", msg::node::FIELD);
            };
            let ident_str = field
                .syn
                .ident
                .as_ref()
                .map(|f| f.to_string())
                .unwrap_or_else(|| format!("field{}", field.nth));
            let ident = syn::Ident::new(&ident_str, Span::call_site());
            let ident_tc_str = to_titlecase(&ident_str);
            let ident_tc = syn::Ident::new(&ident_tc_str, Span::call_site());
            (field_node, ident, ident_tc)
        };
        let builder_data = traverse(
            graph,
            Some(&[&StructRelation::FieldTrain]),
            *ix,
            true,
            action,
        );

        /* ✅ #TD52440080 Create builder fields, generics and their relations. */
        let mut predecessor_ident = None;
        let mut predecessor_ident_tc = None;
        for (i, (field_node, ident, ident_tc)) in builder_data.into_iter().enumerate() {
            /* ✅ #TD72341053 Field */
            let successor_field = graph.add_node(StructElement::BuilderField(Rc::new(ident)));
            if let Some(predecessor) = predecessor_ident.take() {
                graph.add_edge(
                    predecessor,
                    successor_field,
                    StructRelation::BuilderFieldTrain,
                );
            }
            predecessor_ident.get_or_insert(successor_field);
            let node_string = stringify!(BuilderField);
            let key = format!("{}{}", node_string, i);
            map.insert(key, successor_field);
            graph.add_edge(
                field_node,
                successor_field,
                StructRelation::FieldToBuilderField,
            );

            /* ✅ #TD45099623 Generic */
            let successor_generic =
                graph.add_node(StructElement::BuilderGeneric(Rc::new(ident_tc)));
            if let Some(predecessor) = predecessor_ident_tc.take() {
                graph.add_edge(
                    predecessor,
                    successor_generic,
                    StructRelation::BuilderGenericTrain,
                );
            }
            predecessor_ident_tc.get_or_insert(successor_generic);
            let node_string = stringify!(BuilderGeneric);
            let key = format!("{}{}", node_string, i);
            map.insert(key, successor_generic);
            graph.add_edge(
                successor_field,
                successor_generic,
                StructRelation::BuilderFieldToBuilderGeneric,
            );
        }
    }
}

fn create_builder_states(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    /* ✅ #TD44491595 Traverse on field train. */
    if let Some(start) = map.get(mapkey::startp::FIELD) {
        let action = |graph: &mut StructGraph, _edge, field_node| {
            /* ✅ #TD78515467 All data to create two-legged state structs. */
            let BuilderStatePair {
                main_ident,
                ident,
                ty,
                field_to_main_lifetimes,
                field_to_main_consts,
                field_to_main_types,
                field_to_where_predicates,
            } = BuilderStatePair::new(graph, field_node, map);

            /* ✅ #TD93602268 Idents of pair. */
            let ident_empty =
                format_ident!("{}{}Empty", main_ident, to_titlecase(&ident.to_string()));
            let ident_added =
                format_ident!("{}{}Added", main_ident, to_titlecase(&ident.to_string()));

            /* ✅ #TD39331204 Create field generics. */
            let len_additions = field_to_where_predicates.len() * 2;
            let len_generics = field_to_main_lifetimes.len()
                + field_to_main_consts.len()
                + field_to_main_types.len()
                + len_additions;
            let mut generics = Vec::with_capacity(len_generics);
            let mut generics_additions = Vec::with_capacity(len_generics);

            /* ✅ #TD13775189 Phantoms init */
            let mut phantoms = Vec::with_capacity(field_to_where_predicates.len());

            /* ✅ #TD18715806 Determining where predicates related to the field. */
            let mut where_predicates = Vec::with_capacity(field_to_where_predicates.len());
            for field_to_where_predicate in field_to_where_predicates {
                let mut predicate = (*field_to_where_predicate.wp).clone();

                /* 🌀 COMPLEXITY #CP60692702 Orphan wp right lifetimes. Add Higher-ranked trait bounds for them (ForLifetimes). */
                {
                    let filter = field_to_where_predicate
                        .right_lifetimes_in_generics
                        .into_iter()
                        .filter(|p0| {
                            !field_to_main_lifetimes
                                .iter()
                                .any(|p1| compare_generic_params(p0, p1))
                        })
                        .map(|f| (*f).clone())
                        .collect::<Vec<_>>();

                    match &mut predicate {
                        syn::WherePredicate::Lifetime(_predicate_lifetime) => unimplemented!(),
                        syn::WherePredicate::Type(predicate_type) => {
                            if let Some(ptyb) = &mut predicate_type.lifetimes {
                                ptyb.lifetimes.extend(filter);
                            } else if !filter.is_empty() {
                                let mut lts = syn::BoundLifetimes::default();
                                lts.lifetimes.extend(filter);
                                predicate_type.lifetimes = Some(lts);
                            }
                        }
                        _ => unimplemented!(),
                    }
                }

                /* 🌀 COMPLEXITY #CP64472417 Orphan wp right types. Add type parameters in the main generics. */
                {
                    let filter = field_to_where_predicate
                        .right_types_in_generics
                        .into_iter()
                        .filter(|p0| {
                            !field_to_main_types
                                .iter()
                                .any(|p1| compare_generic_params(p0, p1))
                        })
                        .collect::<Vec<_>>();
                    generics_additions.extend(filter);
                }

                /* 🌀 COMPLEXITY #CP34264506 Using phantoms is needed some sub generic parameters. Add type parameters in the main generics and in the tuple as phantom data. */
                {
                    let filter = field_to_where_predicate
                        .right_types_phantoms_in_generics
                        .into_iter()
                        .filter(|p0| {
                            !field_to_main_types
                                .iter()
                                .any(|p1| compare_generic_params(p0, p1))
                        })
                        .collect::<Vec<_>>();
                    for phantom in filter.iter() {
                        phantoms.push(Rc::clone(phantom));
                    }
                    generics_additions.extend(filter);
                }

                /* ✅ #TD60868169 Finally push produced predicate. */
                where_predicates.push(predicate);
            }

            /* ✅ #TD76108810 Determining main generics related to the field. */
            generics.extend(field_to_main_lifetimes);
            generics.extend(field_to_main_consts);
            generics.extend(field_to_main_types);
            generics.extend(generics_additions);

            /* ✅ #TD51147690 Add nodes and the pair edge. */
            let ty = ty.clone();
            let ix_state_empty =
                graph.add_node(StructElement::BuilderStateEmpty(Rc::new(ident_empty)));
            let ix_state_added = graph.add_node(StructElement::BuilderStateAdded(Rc::new(
                BuilderStateAdded {
                    ident: ident_added,
                    generics,
                    ty,
                    where_predicates,
                    phantoms,
                },
            )));
            graph.add_edge(
                ix_state_empty,
                ix_state_added,
                StructRelation::BuilderStatePair,
            );

            /* ✅ #TD64139852 Traverse to builder field to connect to the state pair. */
            let action = |graph: &mut StructGraph, _edge, builder_field_node| {
                graph.add_edge(
                    builder_field_node,
                    ix_state_empty,
                    StructRelation::BuilderFieldToBuilderState,
                );
            };
            traverse_mut(
                graph,
                Some(&[&StructRelation::FieldToBuilderField]),
                field_node,
                false,
                action,
            );
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

fn compare_generic_params(p0: &Rc<syn::GenericParam>, p1: &Rc<syn::GenericParam>) -> bool {
    match (p0.as_ref(), p1.as_ref()) {
        (
            syn::GenericParam::Lifetime(lifetime_param0),
            syn::GenericParam::Lifetime(lifetime_param1),
        ) => lifetime_param0.lifetime == lifetime_param1.lifetime,
        (syn::GenericParam::Type(type_param0), syn::GenericParam::Type(type_param1)) => {
            type_param0.ident == type_param1.ident
        }
        _ => false,
    }
}

struct FieldToWherePredicate {
    wp: Rc<syn::WherePredicate>,
    right_lifetimes_in_generics: Vec<Rc<syn::GenericParam>>,
    right_types_in_generics: Vec<Rc<syn::GenericParam>>,
    right_types_phantoms_in_generics: Vec<Rc<syn::GenericParam>>,
}

struct BuilderStatePair<'a> {
    main_ident: syn::Ident,
    ident: Cow<'a, syn::Ident>,
    ty: &'a syn::Type,
    field_to_main_lifetimes: Vec<Rc<syn::GenericParam>>,
    field_to_main_consts: Vec<Rc<syn::GenericParam>>,
    field_to_main_types: Vec<Rc<syn::GenericParam>>,
    field_to_where_predicates: Vec<FieldToWherePredicate>,
}

impl<'a> BuilderStatePair<'a> {
    fn new(
        graph: &'a mut StructGraph,
        field_node: NodeIndex,
        map: &'a IndexMap<String, NodeIndex>,
    ) -> Self {
        let StructElement::Field(field) = &graph[field_node] else {
            panic!("{}", msg::node::FIELD);
        };
        let Some(StructElement::Ident(main_ident)) = map.get("Ident").map(|f| &graph[*f]) else {
            panic!("Struct must have an ident.");
        };
        let main_ident = format_ident!("{}Builder", main_ident);
        let ident = if let Some(ident) = &field.syn.ident {
            Cow::Borrowed(ident)
        } else {
            Cow::Owned(syn::Ident::new(
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
            panic!("{}", msg::node::GENERIC);
        };
        Rc::clone(&generic.syn)
    }

    fn traverse_field_to_where_predicate(
        graph: &StructGraph,
        _edge: Option<EdgeIndex>,
        wp_node: NodeIndex,
    ) -> FieldToWherePredicate {
        let StructElement::WherePredicate(wp) = &graph[wp_node] else {
            panic!("{}", msg::node::WP);
        };
        let right_lifetimes_in_generics = traverse(
            graph,
            Some(&[&StructRelation::WPRightBoundingLifetimeInMain]),
            wp_node,
            false,
            Self::traverse_wp_to_main_generics,
        );
        let right_types_in_generics = traverse(
            graph,
            Some(&[&StructRelation::WPRightBoundingTypeInMain]),
            wp_node,
            false,
            Self::traverse_wp_to_main_generics,
        );
        let right_types_phantoms_in_generics = traverse(
            graph,
            Some(&[&StructRelation::WPRightBoundingTypePhantomInMain]),
            wp_node,
            false,
            Self::traverse_wp_to_main_generics,
        );
        FieldToWherePredicate {
            wp: Rc::clone(&wp.syn),
            right_lifetimes_in_generics,
            right_types_in_generics,
            right_types_phantoms_in_generics,
        }
    }

    fn traverse_wp_to_main_generics(
        graph: &StructGraph,
        _edge: Option<EdgeIndex>,
        generic_node: NodeIndex,
    ) -> Rc<syn::GenericParam> {
        let StructElement::Generic(generic) = &graph[generic_node] else {
            panic!("{}", msg::node::GENERIC);
        };
        Rc::clone(&generic.syn)
    }
}
