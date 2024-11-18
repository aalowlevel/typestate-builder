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

use indexmap::IndexMap;
use petgraph::graph::NodeIndex;

use crate::{
    graph::{mapkey, msg, traverse_mut, StructElement, StructGraph, StructRelation},
    helper::extract_ident,
};

pub fn run(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    bind_field_elements(graph, map);
    bind_where_predicate_elements(graph, map);
}

fn bind_field_elements(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    let start = map.get(mapkey::startp::FIELD).cloned();
    if let Some(start) = start {
        let action = |graph: &mut StructGraph, _edge, node_field| {
            list_field_assets(graph, node_field);
            traversal_field_to_generics(graph, node_field, map);
            traversal_field_to_where_clause(graph, node_field, map);
        };
        let _: () = traverse_mut(graph, &[&StructRelation::FieldTrain], start, true, action);
    }
}

fn bind_where_predicate_elements(graph: &mut StructGraph, map: &mut IndexMap<String, NodeIndex>) {
    let start = map.get(mapkey::startp::WP).cloned();
    if let Some(start) = start {
        let action = |graph: &mut StructGraph, _edge, node_wp| {
            list_wp_assets(graph, node_wp);
            traversal_wp_to_generics(graph, node_wp, map);
        };
        let _: () = traverse_mut(
            graph,
            &[&StructRelation::WherePredicateTrain],
            start,
            true,
            action,
        );
    }
}

fn list_field_assets(graph: &mut StructGraph, node_field: NodeIndex) {
    let StructElement::Field(field) = &mut graph[node_field] else {
        panic!("{}", msg::node::FIELD);
    };
    field.list();
}
fn list_wp_assets(graph: &mut StructGraph, wp_field: NodeIndex) {
    let StructElement::WherePredicate(wp) = &mut graph[wp_field] else {
        panic!("{}", msg::node::WP);
    };
    let (
        left_bound_lifetimes,
        left_bounded_type,
        left_bounded_lifetime,
        right_bounding_types,
        right_bounding_lifetimes,
        right_bounding_phantoms,
    ) = wp.list();
    wp.left_bound_lifetimes = left_bound_lifetimes;
    wp.left_bounded_type = left_bounded_type;
    wp.left_bounded_lifetime = left_bounded_lifetime;
    wp.right_bounding_types = right_bounding_types;
    wp.right_bounding_lifetimes = right_bounding_lifetimes;
    wp.right_bounding_phantoms = right_bounding_phantoms;
}
fn traversal_field_to_generics(
    graph: &mut StructGraph,
    node_field: NodeIndex,
    map: &mut IndexMap<String, NodeIndex>,
) {
    if let Some(start) = map.get(mapkey::startp::GENERICS) {
        let action = |graph: &mut StructGraph, _edge, node_generic| {
            search_in_generics_by_field(graph, node_field, node_generic);
        };
        let _: () = traverse_mut(
            graph,
            &[&StructRelation::GenericTrain],
            *start,
            true,
            action,
        );
    }
}
/** Checks whether any element in the field is defined in the generics. If it is defined, establishes a connection. */
fn search_in_generics_by_field(
    graph: &mut StructGraph,
    node_field: NodeIndex,
    node_generic: NodeIndex,
) {
    let StructElement::Generic(generic) = &graph[node_generic] else {
        panic!("{}", msg::node::GENERIC);
    };
    let generic_ident = match generic.syn.as_ref() {
        syn::GenericParam::Lifetime(lifetime_param) => &lifetime_param.lifetime.ident,
        syn::GenericParam::Type(type_param) => &type_param.ident,
        syn::GenericParam::Const(const_param) => &const_param.ident,
    };
    let StructElement::Field(field) = &graph[node_field] else {
        panic!("{}", msg::node::FIELD);
    };
    let type_found = field
        .types
        .iter()
        .any(|type_ident| type_ident == generic_ident);

    let lifetime_found = field
        .lifetimes
        .iter()
        .any(|lifetime| &lifetime.ident == generic_ident);
    let const_param_found = field
        .const_params
        .iter()
        .any(|const_param_ident| const_param_ident == generic_ident);
    if type_found {
        graph.add_edge(
            node_field,
            node_generic,
            StructRelation::FieldGenericInMainType,
        );
    }
    if lifetime_found {
        graph.add_edge(
            node_field,
            node_generic,
            StructRelation::FieldGenericInMainLifetime,
        );
    }
    if const_param_found {
        graph.add_edge(
            node_field,
            node_generic,
            StructRelation::FieldGenericInMainConst,
        );
    }
}

fn traversal_field_to_where_clause(
    graph: &mut StructGraph,
    node_field: NodeIndex,
    map: &mut IndexMap<String, NodeIndex>,
) {
    if let Some(start) = map.get(mapkey::startp::WP) {
        let action = |graph: &mut StructGraph, _edge, node_wp| {
            search_in_wp_by_field(graph, node_field, node_wp);
        };

        let _: () = traverse_mut(
            graph,
            &[&StructRelation::WherePredicateTrain],
            *start,
            true,
            action,
        );
    }
}

/** Checks whether any element in the field is defined in where clause of the generics. If it is defined, establishes a connection. */
fn search_in_wp_by_field(graph: &mut StructGraph, node_field: NodeIndex, node_wp: NodeIndex) {
    let StructElement::WherePredicate(wp) = &graph[node_wp] else {
        panic!("{}", msg::node::WP);
    };
    let wp_ident = match wp.syn.as_ref() {
        syn::WherePredicate::Lifetime(predicate_lifetime) => {
            Some(&predicate_lifetime.lifetime.ident)
        }
        syn::WherePredicate::Type(predicate_type) => extract_ident(&predicate_type.bounded_ty),
        _ => None,
    };
    let StructElement::Field(field) = &graph[node_field] else {
        panic!("{}", msg::node::FIELD);
    };
    let found = field
        .types
        .iter()
        .any(|type_ident| Some(type_ident) == wp_ident)
        || field
            .lifetimes
            .iter()
            .any(|lifetime| Some(&lifetime.ident) == wp_ident)
        || field
            .const_params
            .iter()
            .any(|const_param_ident| Some(const_param_ident) == wp_ident);
    if found {
        graph.add_edge(
            node_field,
            node_wp,
            StructRelation::FieldGenericInWhereClause,
        );
    }
}
fn traversal_wp_to_generics(
    graph: &mut StructGraph,
    node_wp: NodeIndex,
    map: &mut IndexMap<String, NodeIndex>,
) {
    if let Some(start) = map.get(mapkey::startp::GENERICS) {
        let action = |graph: &mut StructGraph, _edge, node_generic| {
            search_in_generics_by_wp(graph, node_wp, node_generic);
        };

        let _: () = traverse_mut(
            graph,
            &[&StructRelation::GenericTrain],
            *start,
            true,
            action,
        );
    }
}
fn search_in_generics_by_wp(graph: &mut StructGraph, node_wp: NodeIndex, node_generic: NodeIndex) {
    let StructElement::Generic(generic) = &graph[node_generic] else {
        panic!("{}", msg::node::GENERIC);
    };
    let StructElement::WherePredicate(wp) = &graph[node_wp] else {
        panic!("{}", msg::node::WP);
    };
    match generic.syn.as_ref() {
        syn::GenericParam::Lifetime(lifetime_param) => {
            let mut left = false;
            let mut right = false;

            if let Some(lt_generic) = &wp.left_bounded_lifetime {
                left = lt_generic == &lifetime_param.lifetime;
            }
            if let Some(lts_wp) = &wp.right_bounding_lifetimes {
                for lt_wp in lts_wp {
                    if lt_wp == &lifetime_param.lifetime {
                        right = true;
                    }
                }
            }

            if left {
                graph.add_edge(
                    node_wp,
                    node_generic,
                    StructRelation::WPLeftBoundedLifetimeInMain,
                );
            }
            if right {
                graph.add_edge(
                    node_wp,
                    node_generic,
                    StructRelation::WPRightBoundingLifetimeInMain,
                );
            }
        }
        syn::GenericParam::Type(type_param) => {
            let mut left = false;
            let mut right = false;
            let mut right_phantom = false;

            if let Some(syn::Type::Path(type_path)) = &wp.left_bounded_type {
                if let Some(ident) = type_path.path.get_ident() {
                    left = ident == &type_param.ident;
                }
            }
            if let Some(tys_wp) = &wp.right_bounding_types {
                for ty_wp in tys_wp {
                    if let syn::Type::Path(type_path) = ty_wp {
                        if let Some(ident) = type_path.path.get_ident() {
                            if ident == &type_param.ident {
                                right = true;
                            }
                        }
                    }
                }
            }
            if let Some(tys_wp) = &wp.right_bounding_phantoms {
                for ty_wp in tys_wp {
                    if let syn::Type::Path(type_path) = ty_wp {
                        if let Some(ident) = type_path.path.get_ident() {
                            if ident == &type_param.ident {
                                right_phantom = true;
                            }
                        }
                    }
                }
            }
            if left {
                graph.add_edge(
                    node_wp,
                    node_generic,
                    StructRelation::WPLeftBoundedTypeInMain,
                );
            }
            if right {
                graph.add_edge(
                    node_wp,
                    node_generic,
                    StructRelation::WPRightBoundingTypeInMain,
                );
            }
            if right_phantom {
                graph.add_edge(
                    node_wp,
                    node_generic,
                    StructRelation::WPRightBoundingTypePhantomInMain,
                );
            }
        }
        syn::GenericParam::Const(_const_param) => {}
    }
}
