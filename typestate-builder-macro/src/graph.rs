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

use std::collections::VecDeque;

use indexmap::IndexSet;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};
use serde::{ser::SerializeStruct, Serialize};
use serde_json::json;
use syn::{
    Attribute, Expr, ExprPath, GenericArgument, Ident, Lifetime, PathArguments, Type, TypeArray,
    Visibility,
};

pub const FIELD_START_P: &str = "Field0";
pub const GENERICS_START_P: &str = "Generic0";
pub const WHERE_PREDICATE_START_P: &str = "WherePredicate0";

pub enum StructElement {
    Visibility(Visibility),
    Ident(Ident),
    Attribute(Attribute),
    Generic(GenericParam),
    WherePredicate(WherePredicate),
    Field(Field),
}

impl Serialize for StructElement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            StructElement::Visibility(_visibility) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "Visibility",
                &json!("Visibility"),
            ),
            StructElement::Ident(_ident) => {
                serializer.serialize_newtype_variant("StructElement", 0, "Ident", &json!("Ident"))
            }
            StructElement::Attribute(_attribute) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "Attribute",
                &json!("Attribute"),
            ),
            StructElement::Generic(generic_param) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "GenericParam",
                &generic_param,
            ),
            StructElement::WherePredicate(where_predicate) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "WherePredicate",
                &where_predicate,
            ),
            StructElement::Field(field) => {
                serializer.serialize_newtype_variant("StructElement", 0, "Field", &field)
            }
        }
    }
}

impl std::fmt::Debug for StructElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            serde_json::to_string(self).expect("serialize to string pretty")
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum StructRelation {
    AttributeTrain,
    GenericTrain,
    WherePredicateTrain,
    FieldTrain,
    FieldGenericsInMain,
    FieldGenericsInWhereClause,
}

pub type StructGraph = Graph<StructElement, StructRelation>;

pub struct Field {
    pub nth: usize,
    pub syn: syn::Field,
    pub idents: IndexSet<Ident>,
    pub lifetimes: IndexSet<Lifetime>,
    pub const_params: IndexSet<Ident>,
}

impl Serialize for Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("Field", 1)?;
        res.serialize_field("nth", &self.nth)?;
        res.skip_field("syn")?;
        res.end()
    }
}

impl Field {
    pub fn list(&mut self) {
        Self::list_type(
            &self.syn.ty,
            &mut self.idents,
            &mut self.lifetimes,
            &mut self.const_params,
        );
    }

    fn list_type(
        ty: &Type,
        idents: &mut IndexSet<Ident>,
        lifetimes: &mut IndexSet<Lifetime>,
        const_params: &mut IndexSet<Ident>,
    ) {
        match ty {
            Type::Array(TypeArray { elem, len, .. }) => {
                Self::list_type(elem, idents, lifetimes, const_params);
                Self::handle_const_expr(len, const_params);
            }
            Type::BareFn(type_bare_fn) => {
                for input in &type_bare_fn.inputs {
                    Self::list_type(&input.ty, idents, lifetimes, const_params);
                }
                if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                    Self::list_type(return_type, idents, lifetimes, const_params);
                }
            }
            Type::Group(type_group) => {
                Self::list_type(&type_group.elem, idents, lifetimes, const_params)
            }
            Type::ImplTrait(type_impl_trait) => {
                for bound in &type_impl_trait.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            Type::Macro(type_macro) => {
                if let Some(ident) = type_macro.mac.path.get_ident() {
                    idents.insert(ident.clone());
                }
            }
            Type::Paren(type_paren) => {
                Self::list_type(&type_paren.elem, idents, lifetimes, const_params)
            }
            Type::Path(type_path) => {
                Self::handle_path(&type_path.path, idents, lifetimes, const_params);
                if let Some(qself) = &type_path.qself {
                    Self::list_type(&qself.ty, idents, lifetimes, const_params);
                }
            }
            Type::Ptr(type_ptr) => Self::list_type(&type_ptr.elem, idents, lifetimes, const_params),
            Type::Reference(type_reference) => {
                if let Some(lt) = &type_reference.lifetime {
                    lifetimes.insert(lt.clone());
                }
                Self::list_type(&type_reference.elem, idents, lifetimes, const_params)
            }
            Type::Slice(type_slice) => {
                Self::list_type(&type_slice.elem, idents, lifetimes, const_params)
            }
            Type::TraitObject(type_trait_object) => {
                for bound in &type_trait_object.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            Type::Tuple(type_tuple) => {
                for elem in &type_tuple.elems {
                    Self::list_type(elem, idents, lifetimes, const_params);
                }
            }
            _ => {}
        }
    }

    fn handle_path(
        path: &syn::Path,
        idents: &mut IndexSet<Ident>,
        lifetimes: &mut IndexSet<Lifetime>,
        const_params: &mut IndexSet<Ident>,
    ) {
        for segment in &path.segments {
            idents.insert(segment.ident.clone());
            match &segment.arguments {
                PathArguments::AngleBracketed(args) => {
                    for arg in &args.args {
                        Self::handle_generic_argument(arg, idents, lifetimes, const_params);
                    }
                }
                PathArguments::Parenthesized(args) => {
                    for ty in &args.inputs {
                        Self::list_type(ty, idents, lifetimes, const_params);
                    }
                    if let syn::ReturnType::Type(_, ty) = &args.output {
                        Self::list_type(ty, idents, lifetimes, const_params);
                    }
                }
                PathArguments::None => {}
            }
        }
    }

    fn handle_generic_argument(
        arg: &GenericArgument,
        idents: &mut IndexSet<Ident>,
        lifetimes: &mut IndexSet<Lifetime>,
        const_params: &mut IndexSet<Ident>,
    ) {
        match arg {
            GenericArgument::Type(ty) => Self::list_type(ty, idents, lifetimes, const_params),
            GenericArgument::Lifetime(lt) => {
                lifetimes.insert(lt.clone());
            }
            GenericArgument::Const(expr) => Self::handle_const_expr(expr, const_params),
            GenericArgument::Constraint(constraint) => {
                idents.insert(constraint.ident.clone());
                for bound in &constraint.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            _ => {}
        }
    }

    fn handle_const_expr(expr: &Expr, const_params: &mut IndexSet<Ident>) {
        if let Expr::Path(ExprPath { path, .. }) = expr {
            if path.segments.len() == 1 {
                // Single segment path is likely a const generic parameter
                const_params.insert(path.segments[0].ident.clone());
            }
            // For multi-segment paths, we don't add to const_params
            // as they're likely not const generic parameters
        }
    }

    fn handle_type_param_bound(
        bound: &syn::TypeParamBound,
        idents: &mut IndexSet<Ident>,
        lifetimes: &mut IndexSet<Lifetime>,
        const_params: &mut IndexSet<Ident>,
    ) {
        match bound {
            syn::TypeParamBound::Trait(trait_bound) => {
                Self::handle_path(&trait_bound.path, idents, lifetimes, const_params);
            }
            syn::TypeParamBound::Lifetime(lt) => {
                lifetimes.insert(lt.clone());
            }
            _ => {}
        }
    }
}

pub struct GenericParam {
    pub nth: usize,
    pub syn: syn::GenericParam,
}

impl Serialize for GenericParam {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("GenericParam", 1)?;
        res.serialize_field("nth", &self.nth)?;
        res.skip_field("syn")?;
        res.end()
    }
}

pub struct WherePredicate {
    pub nth: usize,
    pub syn: syn::WherePredicate,
}

impl Serialize for WherePredicate {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("WherePredicate", 1)?;
        res.serialize_field("nth", &self.nth)?;
        res.skip_field("syn")?;
        res.end()
    }
}

pub fn traverse_by_edge<'a, F>(
    graph: &'a StructGraph,
    edge_type: &'a StructRelation,
    start: NodeIndex,
    mut node_action: F,
) -> IndexSet<NodeIndex>
where
    F: FnMut(&StructGraph, EdgeIndex, NodeIndex),
{
    let mut queue = VecDeque::new();
    let mut visited = IndexSet::new();
    queue.push_back(start);

    while let Some(node) = queue.pop_front() {
        if visited.insert(node) {
            let neighbors: Vec<_> = graph
                .neighbors(node)
                .filter_map(|neighbor| {
                    graph.find_edge(node, neighbor).and_then(|edge| {
                        if &graph[edge] == edge_type {
                            Some((edge, neighbor))
                        } else {
                            None
                        }
                    })
                })
                .collect();

            for (edge, neighbor) in neighbors {
                node_action(graph, edge, neighbor);
                if !visited.contains(&neighbor) {
                    queue.push_back(neighbor);
                }
            }
        }
    }
    visited
}

pub fn traverse_by_edge_mut<'a, F>(
    graph: &'a mut StructGraph,
    edge_type: &'a StructRelation,
    start_node: NodeIndex,
    mut node_action: F,
) -> IndexSet<NodeIndex>
where
    F: FnMut(&mut StructGraph, EdgeIndex, NodeIndex),
{
    let mut queue = VecDeque::new();
    let mut visited = IndexSet::new();
    queue.push_back(start_node);

    while let Some(node) = queue.pop_front() {
        if visited.insert(node) {
            let neighbors: Vec<_> = graph
                .neighbors(node)
                .filter_map(|neighbor| {
                    graph.find_edge(node, neighbor).and_then(|edge| {
                        if &graph[edge] == edge_type {
                            Some((edge, neighbor))
                        } else {
                            None
                        }
                    })
                })
                .collect();

            for (edge, neighbor) in neighbors {
                node_action(graph, edge, neighbor);
                if !visited.contains(&neighbor) {
                    queue.push_back(neighbor);
                }
            }
        }
    }
    visited
}
