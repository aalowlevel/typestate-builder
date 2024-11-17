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

use indexmap::IndexSet;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use serde::{ser::SerializeStruct, Serialize};
use serde_json::json;

pub mod mapkey {
    pub mod startp {
        pub const FIELD: &str = "Field0";
        pub const GENERICS: &str = "Generic0";
        pub const WP: &str = "WherePredicate0";
        pub const BUILDER_FIELD: &str = "BuilderField0";
        pub const FEATURE_DEFAULT: &str = "FeatureDefault0";
    }
    pub mod uniq {
        pub const VIS: &str = "Visibility";
        pub const IDENT: &str = "Ident";
        pub const TYPE: &str = "Type";
        pub const METHOD_BUILDER_IDENT: &str = "MethodBuilderIdent";
        pub const BUILDER_IDENT: &str = "BuilderIdent";
    }
}

pub mod msg {
    pub mod ix {
        pub const IDENT: &str = "There must be ident node.";
        pub const METHOD_BUILDER_IDENT: &str = "There must be method builder ident node.";
        pub const BUILDER_IDENT: &str = "There must be builder ident node.";
        pub const VIS: &str = "There must be visibility node.";
        pub const TYPE: &str = "There must be type node.";
    }
    pub mod node {
        pub const VIS: &str = "Node must be a visibility.";
        pub const IDENT: &str = "Node must be an ident.";
        pub const TYPE: &str = "Node must be a type.";
        pub const FIELD: &str = "Node must be a field.";
        pub const GENERIC: &str = "Node must be a generic.";
        pub const WP: &str = "Node must be a Where Predicate.";
        pub const METHOD_BUILDER_IDENT: &str = "Node must be a method builder ident.";
        pub const BUILDER_IDENT: &str = "Node must be a builder ident.";
        pub const FEATURE_DEFAULT: &str = "Node must be a Feature Default.";
    }
}

pub enum StructElement {
    Visibility(syn::Visibility),
    Ident(syn::Ident),
    Generic(GenericParam),
    WherePredicate(WherePredicate),
    Field(Field),
    Type(StructType),
    MethodBuilderIdent(Rc<syn::Ident>),
    BuilderStateEmpty(Rc<syn::Ident>),
    BuilderStateAdded(Rc<BuilderStateAdded>),
    BuilderIdent(Rc<syn::Ident>),
    BuilderField(Rc<syn::Ident>),
    BuilderGeneric(Rc<syn::Ident>),
    FeatureDefault(FeatureDefault),
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
            StructElement::Type(struct_type) => {
                serializer.serialize_newtype_variant("StructElement", 0, "Type", &struct_type)
            }
            StructElement::MethodBuilderIdent(ident) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "MethodBuilderIdent",
                &json!(ident.to_string()),
            ),
            StructElement::BuilderStateEmpty(ident) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "BuilderStateEmpty",
                &json!(ident.to_string()),
            ),
            StructElement::BuilderStateAdded(builder_state_added) => serializer
                .serialize_newtype_variant(
                    "StructElement",
                    0,
                    "BuilderStateAdded",
                    builder_state_added.as_ref(),
                ),
            StructElement::BuilderIdent(ident) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "BuilderIdent",
                &json!(ident.to_string()),
            ),
            StructElement::BuilderField(ident) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "BuilderField",
                &json!(ident.to_string()),
            ),
            StructElement::BuilderGeneric(ident) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "BuilderGeneric",
                &json!(ident.to_string()),
            ),
            StructElement::FeatureDefault(feature_default) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "FeatureDefault",
                feature_default,
            ),
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

#[derive(Serialize)]
pub enum StructType {
    Named,
    Unnamed,
}

#[derive(Debug, PartialEq)]
pub enum StructRelation {
    GenericTrain,
    WherePredicateTrain,
    FieldTrain,
    FieldGenericInMainType,
    FieldGenericInMainLifetime,
    FieldGenericInMainConst,
    FieldGenericInWhereClause,
    FieldToBuilderField,
    WPLeftBoundedTypeInMain,
    WPLeftBoundedLifetimeInMain,
    WPRightBoundingTypeInMain,
    WPRightBoundingTypePhantomInMain,
    WPRightBoundingLifetimeInMain,
    BuilderFieldTrain,
    BuilderFieldToBuilderGeneric,
    BuilderFieldToBuilderState,
    BuilderGenericTrain,
    BuilderStatePair,
    FeatureDefaultTrain,
}

pub type StructGraph = Graph<StructElement, StructRelation>;

pub struct Field {
    pub nth: usize,
    pub syn: syn::Field,
    pub types: IndexSet<syn::Ident>,
    pub lifetimes: IndexSet<syn::Lifetime>,
    pub const_params: IndexSet<syn::Ident>,
    pub default: bool,
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

/* ♻️ REFACTOR #RF45190356 Eliminate Option<Vec<T>>'s */
pub struct WherePredicate {
    pub nth: usize,
    pub syn: Rc<syn::WherePredicate>,
    pub left_bound_lifetimes: Option<Vec<syn::Lifetime>>,
    pub left_bounded_type: Option<syn::Type>,
    pub left_bounded_lifetime: Option<syn::Lifetime>,
    pub right_bounding_types: Option<Vec<syn::Type>>,
    pub right_bounding_lifetimes: Option<Vec<syn::Lifetime>>,
    pub right_bounding_phantoms: Option<Vec<syn::Type>>,
}
pub type WherePredicateInner = (
    Option<Vec<syn::Lifetime>>,
    Option<syn::Type>,
    Option<syn::Lifetime>,
    Option<Vec<syn::Type>>,
    Option<Vec<syn::Lifetime>>,
    Option<Vec<syn::Type>>,
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

                let (right_bounding_types, right_bounding_lifetimes, right_bounding_phantoms) =
                    Self::process_bounds(bounds);

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
                    if right_bounding_phantoms.is_empty() {
                        None
                    } else {
                        Some(right_bounding_phantoms)
                    },
                )
            }
            syn::WherePredicate::Lifetime(predicate_lifetime) => (
                None,
                None,
                Some(predicate_lifetime.lifetime.clone()),
                None,
                Some(predicate_lifetime.bounds.iter().cloned().collect()),
                None,
            ),
            _ => (None, None, None, None, None, None),
        }
    }

    fn process_bounds(
        bounds: &syn::punctuated::Punctuated<syn::TypeParamBound, syn::token::Plus>,
    ) -> (Vec<syn::Type>, Vec<syn::Lifetime>, Vec<syn::Type>) {
        bounds.iter().fold(
            (
                Vec::with_capacity(bounds.len()),
                Vec::with_capacity(bounds.len()),
                Vec::with_capacity(bounds.len()),
            ),
            |(mut types, mut lifetimes, mut phantoms), bound| {
                match bound {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        Self::process_trait_bound(trait_bound, &mut types, &mut phantoms);
                    }
                    syn::TypeParamBound::Lifetime(lifetime) => {
                        lifetimes.push(lifetime.clone());
                    }
                    _ => {}
                }
                (types, lifetimes, phantoms)
            },
        )
    }

    fn process_trait_bound(
        trait_bound: &syn::TraitBound,
        types: &mut Vec<syn::Type>,
        phantoms: &mut Vec<syn::Type>,
    ) {
        if let Some(segment) = trait_bound.path.segments.last() {
            if segment.ident == "Fn" || segment.ident == "FnMut" || segment.ident == "FnOnce" {
                if let syn::PathArguments::Parenthesized(args) = &segment.arguments {
                    for arg in args.inputs.iter() {
                        phantoms.push(arg.clone());
                    }
                    if let syn::ReturnType::Type(_, ty) = &args.output {
                        phantoms.push((**ty).clone());
                    }
                }
            } else {
                types.push(syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: trait_bound.path.clone(),
                }));
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in args.args.iter() {
                        match arg {
                            syn::GenericArgument::Type(ty) => {
                                Self::process_type(ty, types, phantoms);
                            }
                            syn::GenericArgument::Lifetime(_lt) => {
                                // We don't add lifetimes here as they're handled separately
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    fn process_type(ty: &syn::Type, _types: &mut Vec<syn::Type>, phantoms: &mut Vec<syn::Type>) {
        match ty {
            syn::Type::Path(type_path) => {
                if let Some(segment) = type_path.path.segments.last() {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        for arg in args.args.iter() {
                            if let syn::GenericArgument::Type(inner_ty) = arg {
                                Self::process_type(inner_ty, _types, phantoms);
                            }
                        }
                    }
                }
                phantoms.push(ty.clone());
            }
            _ => phantoms.push(ty.clone()),
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

pub struct BuilderStateAdded {
    pub ident: syn::Ident,
    pub generics: Vec<Rc<syn::GenericParam>>,
    pub ty: syn::Type,
    pub where_predicates: Vec<Rc<syn::WherePredicate>>,
    pub phantoms: Vec<TokenStream2>,
    pub orphans: Vec<syn::GenericParam>,
}

impl Serialize for BuilderStateAdded {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("BuilderStateAdded", 5)?;
        res.serialize_field("ident", &self.ident.to_string())?;
        res.skip_field("generics")?;
        res.skip_field("ty")?;
        res.skip_field("where_predicates")?;
        res.skip_field("phantoms")?;
        res.end()
    }
}

pub struct FeatureDefault {
    pub nth: usize,
    pub syn: Rc<syn::PredicateType>,
}

impl Serialize for FeatureDefault {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("FeatureDefault", 2)?;
        res.serialize_field("variant", "Default")?;
        res.serialize_field("nth", &self.nth)?;
        if let syn::Type::Path(path) = &self.syn.bounded_ty {
            if let Some(ident) = path.path.get_ident() {
                res.serialize_field("bounded_ty_ident", &ident.to_string())?;
            }
        }
        res.end()
    }
}

pub fn traverse<'a, N, E, F, R>(
    graph: &'a Graph<N, E>,
    filter_edge: &'a [&'a E],
    start_node: NodeIndex,
    include_start: bool,
    mut node_action: F,
) -> Vec<R>
where
    F: FnMut(&Graph<N, E>, Option<EdgeIndex>, NodeIndex) -> R,
    E: std::cmp::PartialEq,
{
    let capacity = graph.capacity().0;
    let mut stack = Vec::with_capacity(capacity);
    let mut visited = IndexSet::with_capacity(capacity);
    let mut results = Vec::with_capacity(capacity);
    stack.push((start_node, None));

    while let Some((node, edge)) = stack.pop() {
        if !visited.contains(&node) {
            visited.insert(node);

            if edge.is_some() || include_start {
                let result = node_action(graph, edge, node);
                results.push(result);
            }

            let mut neighbors: Vec<_> = graph
                .neighbors(node)
                .filter_map(|neighbor| graph.find_edge(node, neighbor).map(|edge| (edge, neighbor)))
                .filter(|&(edge, _)| filter_edge.contains(&&graph[edge]))
                .collect();

            // Sort neighbors based on filter_edge order
            neighbors.sort_by_key(|&(edge, _)| {
                filter_edge
                    .iter()
                    .position(|&p| p == &graph[edge])
                    .unwrap_or(usize::MAX)
            });
            neighbors.reverse(); // Reverse to maintain priority order when pushing to stack

            for (edge, neighbor) in neighbors {
                if !visited.contains(&neighbor) {
                    stack.push((neighbor, Some(edge)));
                }
            }
        }
    }
    results
}

pub fn traverse_mut<'a, N, E, F, R>(
    graph: &'a mut Graph<N, E>,
    filter_edge: &'a [&'a E],
    start_node: NodeIndex,
    include_start: bool,
    mut node_action: F,
) -> Vec<R>
where
    F: FnMut(&mut Graph<N, E>, Option<EdgeIndex>, NodeIndex) -> R,
    E: std::cmp::PartialEq,
{
    let capacity = graph.capacity().0;
    let mut stack = Vec::with_capacity(capacity);
    let mut visited = IndexSet::with_capacity(capacity);
    let mut results = Vec::with_capacity(capacity);
    stack.push((start_node, None));

    while let Some((node, edge)) = stack.pop() {
        if !visited.contains(&node) {
            visited.insert(node);

            if edge.is_some() || include_start {
                let result = node_action(graph, edge, node);
                results.push(result);
            }

            let mut neighbors: Vec<_> = graph
                .neighbors(node)
                .filter_map(|neighbor| graph.find_edge(node, neighbor).map(|edge| (edge, neighbor)))
                .filter(|&(edge, _)| filter_edge.contains(&&graph[edge]))
                .collect();

            // Sort neighbors based on filter_edge order
            neighbors.sort_by_key(|&(edge, _)| {
                filter_edge
                    .iter()
                    .position(|&p| p == &graph[edge])
                    .unwrap_or(usize::MAX)
            });
            neighbors.reverse(); // Reverse to maintain priority order when pushing to stack

            for (edge, neighbor) in neighbors {
                if !visited.contains(&neighbor) {
                    stack.push((neighbor, Some(edge)));
                }
            }
        }
    }
    results
}
