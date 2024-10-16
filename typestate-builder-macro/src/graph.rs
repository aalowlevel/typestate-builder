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

use std::{collections::VecDeque, rc::Rc};

use indexmap::IndexSet;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};
use quote::ToTokens;
use serde::{ser::SerializeStruct, Serialize};
use serde_json::json;

pub const FIELD_START_P: &str = "Field0";
pub const GENERICS_START_P: &str = "Generic0";
pub const WHERE_PREDICATE_START_P: &str = "WherePredicate0";

pub enum StructElement {
    Visibility(syn::Visibility),
    Ident(syn::Ident),
    Attribute(syn::Attribute),
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
    FieldGenericInMainType,
    FieldGenericInMainLifetime,
    FieldGenericInMainConst,
    FieldGenericInWhereClause,
    WPLeftBoundedTypeInMain,
    WPLeftBoundedLifetimeInMain,
    WPRightBoundingTypeInMain,
    WPRightBoundingLifetimeInMain,
}

pub type StructGraph = Graph<StructElement, StructRelation>;

pub struct Field {
    pub nth: usize,
    pub syn: syn::Field,
    pub types: IndexSet<syn::Ident>,
    pub lifetimes: IndexSet<syn::Lifetime>,
    pub const_params: IndexSet<syn::Ident>,
}

impl Serialize for Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("Field", 3)?;
        let ident = if let Some(ident) = &self.syn.ident {
            ident.to_string()
        } else {
            format!("field{}", self.nth)
        };
        res.serialize_field("nth", &self.nth)?;
        res.serialize_field("f", &ident)?;
        res.serialize_field("ty", &self.syn.ty.to_token_stream().to_string())?;
        res.skip_field("syn")?;
        res.end()
    }
}

impl Field {
    pub fn list(&mut self) {
        Self::list_type(
            &self.syn.ty,
            &mut self.types,
            &mut self.lifetimes,
            &mut self.const_params,
        );
    }

    fn list_type(
        ty: &syn::Type,
        idents: &mut IndexSet<syn::Ident>,
        lifetimes: &mut IndexSet<syn::Lifetime>,
        const_params: &mut IndexSet<syn::Ident>,
    ) {
        match ty {
            syn::Type::Array(syn::TypeArray { elem, len, .. }) => {
                Self::list_type(elem, idents, lifetimes, const_params);
                Self::handle_const_expr(len, const_params);
            }
            syn::Type::BareFn(type_bare_fn) => {
                for input in &type_bare_fn.inputs {
                    Self::list_type(&input.ty, idents, lifetimes, const_params);
                }
                if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                    Self::list_type(return_type, idents, lifetimes, const_params);
                }
            }
            syn::Type::Group(type_group) => {
                Self::list_type(&type_group.elem, idents, lifetimes, const_params)
            }
            syn::Type::ImplTrait(type_impl_trait) => {
                for bound in &type_impl_trait.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            syn::Type::Macro(type_macro) => {
                if let Some(ident) = type_macro.mac.path.get_ident() {
                    idents.insert(ident.clone());
                }
            }
            syn::Type::Paren(type_paren) => {
                Self::list_type(&type_paren.elem, idents, lifetimes, const_params)
            }
            syn::Type::Path(type_path) => {
                Self::handle_path(&type_path.path, idents, lifetimes, const_params);
                if let Some(qself) = &type_path.qself {
                    Self::list_type(&qself.ty, idents, lifetimes, const_params);
                }
            }
            syn::Type::Ptr(type_ptr) => {
                Self::list_type(&type_ptr.elem, idents, lifetimes, const_params)
            }
            syn::Type::Reference(type_reference) => {
                if let Some(lt) = &type_reference.lifetime {
                    lifetimes.insert(lt.clone());
                }
                Self::list_type(&type_reference.elem, idents, lifetimes, const_params)
            }
            syn::Type::Slice(type_slice) => {
                Self::list_type(&type_slice.elem, idents, lifetimes, const_params)
            }
            syn::Type::TraitObject(type_trait_object) => {
                for bound in &type_trait_object.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            syn::Type::Tuple(type_tuple) => {
                for elem in &type_tuple.elems {
                    Self::list_type(elem, idents, lifetimes, const_params);
                }
            }
            _ => {}
        }
    }

    fn handle_path(
        path: &syn::Path,
        idents: &mut IndexSet<syn::Ident>,
        lifetimes: &mut IndexSet<syn::Lifetime>,
        const_params: &mut IndexSet<syn::Ident>,
    ) {
        for segment in &path.segments {
            idents.insert(segment.ident.clone());
            match &segment.arguments {
                syn::PathArguments::AngleBracketed(args) => {
                    for arg in &args.args {
                        Self::handle_generic_argument(arg, idents, lifetimes, const_params);
                    }
                }
                syn::PathArguments::Parenthesized(args) => {
                    for ty in &args.inputs {
                        Self::list_type(ty, idents, lifetimes, const_params);
                    }
                    if let syn::ReturnType::Type(_, ty) = &args.output {
                        Self::list_type(ty, idents, lifetimes, const_params);
                    }
                }
                syn::PathArguments::None => {}
            }
        }
    }

    fn handle_generic_argument(
        arg: &syn::GenericArgument,
        idents: &mut IndexSet<syn::Ident>,
        lifetimes: &mut IndexSet<syn::Lifetime>,
        const_params: &mut IndexSet<syn::Ident>,
    ) {
        match arg {
            syn::GenericArgument::Type(ty) => Self::list_type(ty, idents, lifetimes, const_params),
            syn::GenericArgument::Lifetime(lt) => {
                lifetimes.insert(lt.clone());
            }
            syn::GenericArgument::Const(expr) => Self::handle_const_expr(expr, const_params),
            syn::GenericArgument::Constraint(constraint) => {
                idents.insert(constraint.ident.clone());
                for bound in &constraint.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            _ => {}
        }
    }

    fn handle_const_expr(expr: &syn::Expr, const_params: &mut IndexSet<syn::Ident>) {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = expr {
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
        idents: &mut IndexSet<syn::Ident>,
        lifetimes: &mut IndexSet<syn::Lifetime>,
        const_params: &mut IndexSet<syn::Ident>,
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
    pub syn: Rc<syn::GenericParam>,
}

impl Serialize for GenericParam {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("GenericParam", 2)?;
        res.serialize_field("nth", &self.nth)?;
        res.serialize_field("ty", &self.syn.to_token_stream().to_string())?;
        res.skip_field("syn")?;
        res.end()
    }
}

pub struct WherePredicate {
    pub nth: usize,
    pub syn: Rc<syn::WherePredicate>,
    pub left_bound_lifetimes: Option<Vec<syn::Lifetime>>,
    pub left_bounded_type: Option<syn::Type>,
    pub left_bounded_lifetime: Option<syn::Lifetime>,
    pub right_bounding_types: Option<Vec<syn::Type>>,
    pub right_bounding_lifetimes: Option<Vec<syn::Lifetime>>,
}
pub type WherePredicateInner = (
    Option<Vec<syn::Lifetime>>,
    Option<syn::Type>,
    Option<syn::Lifetime>,
    Option<Vec<syn::Type>>,
    Option<Vec<syn::Lifetime>>,
);

impl WherePredicate {
    pub fn list(&self) -> WherePredicateInner {
        match &*self.syn {
            syn::WherePredicate::Type(predicate_type) => {
                let bounded_ty = &predicate_type.bounded_ty;
                let bounds = &predicate_type.bounds;

                let left_bound_lifetimes =
                    predicate_type.lifetimes.as_ref().map(|generic_params| {
                        generic_params
                            .lifetimes
                            .iter()
                            .filter_map(|param| {
                                if let syn::GenericParam::Lifetime(lifetime_def) = param {
                                    Some(lifetime_def.lifetime.clone())
                                } else {
                                    None
                                }
                            })
                            .collect()
                    });

                let left_bounded_type = Some(bounded_ty.clone());

                let (right_bounding_types, right_bounding_lifetimes) = bounds.iter().fold(
                    (Vec::new(), Vec::new()),
                    |(mut types, mut lifetimes), bound| {
                        match bound {
                            syn::TypeParamBound::Trait(trait_bound) => {
                                types.push(syn::Type::Path(syn::TypePath {
                                    qself: None,
                                    path: trait_bound.path.clone(),
                                }));
                            }
                            syn::TypeParamBound::Lifetime(lifetime) => {
                                lifetimes.push(lifetime.clone());
                            }
                            _ => {}
                        }
                        (types, lifetimes)
                    },
                );

                (
                    left_bound_lifetimes,
                    left_bounded_type,
                    None,
                    if right_bounding_types.is_empty() {
                        None
                    } else {
                        Some(right_bounding_types)
                    },
                    if right_bounding_lifetimes.is_empty() {
                        None
                    } else {
                        Some(right_bounding_lifetimes)
                    },
                )
            }
            syn::WherePredicate::Lifetime(predicate_lifetime) => (
                None,
                None,
                Some(predicate_lifetime.lifetime.clone()),
                None,
                Some(predicate_lifetime.bounds.iter().cloned().collect()),
            ),
            _ => (None, None, None, None, None),
        }
    }
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
pub fn traverse<'a, N, E, F, R>(
    graph: &'a Graph<N, E>,
    filter_edge: Option<&'a [&'a E]>,
    start_node: NodeIndex,
    include_start: bool,
    mut node_action: F,
) -> Vec<R>
where
    F: FnMut(&Graph<N, E>, Option<EdgeIndex>, NodeIndex) -> R,
    E: std::cmp::PartialEq,
{
    let mut queue = VecDeque::new();
    let mut visited = IndexSet::new();
    let mut results = Vec::new();
    queue.push_back(start_node);

    if include_start {
        let result = node_action(graph, None, start_node);
        results.push(result);
    }

    while let Some(node) = queue.pop_front() {
        if visited.insert(node) {
            let neighbors: Vec<_> = graph
                .neighbors(node)
                .filter_map(|neighbor| {
                    graph.find_edge(node, neighbor).and_then(|edge| {
                        if let Some(filter_edge) = filter_edge {
                            filter_edge
                                .iter()
                                .find(|&&p| p == &graph[edge])
                                .map(|_| (edge, neighbor))
                        } else {
                            Some((edge, neighbor))
                        }
                    })
                })
                .collect();

            for (edge, neighbor) in neighbors {
                let result = node_action(graph, Some(edge), neighbor);
                results.push(result);
                if !visited.contains(&neighbor) {
                    queue.push_back(neighbor);
                }
            }
        }
    }
    results
}

pub fn traverse_mut<'a, N, E, F, R>(
    graph: &'a mut Graph<N, E>,
    filter_edge: Option<&'a [&'a E]>,
    start_node: NodeIndex,
    include_start: bool,
    mut node_action: F,
) -> Vec<R>
where
    F: FnMut(&mut Graph<N, E>, Option<EdgeIndex>, NodeIndex) -> R,
    E: std::cmp::PartialEq,
{
    let mut queue = VecDeque::new();
    let mut visited = IndexSet::new();
    let mut results = Vec::new();
    queue.push_back(start_node);

    if include_start {
        let result = node_action(graph, None, start_node);
        results.push(result);
    }

    while let Some(node) = queue.pop_front() {
        if visited.insert(node) {
            let neighbors: Vec<_> = graph
                .neighbors(node)
                .filter_map(|neighbor| {
                    graph.find_edge(node, neighbor).and_then(|edge| {
                        if let Some(filter_edge) = filter_edge {
                            filter_edge
                                .iter()
                                .find(|&&p| p == &graph[edge])
                                .map(|_| (edge, neighbor))
                        } else {
                            Some((edge, neighbor))
                        }
                    })
                })
                .collect();

            for (edge, neighbor) in neighbors {
                let result = node_action(graph, Some(edge), neighbor);
                results.push(result);
                if !visited.contains(&neighbor) {
                    queue.push_back(neighbor);
                }
            }
        }
    }
    results
}
