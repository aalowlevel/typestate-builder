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

#[derive(Debug)]
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
    graph: &'a StructGraph,        // Immutable reference to the graph
    edge_type: &'a StructRelation, // Edge type to filter on
    start: NodeIndex,              // Starting node index
    mut node_action: F,            // Closure that can mutate captured environment
) -> IndexSet<NodeIndex>
// Return visited nodes
where
    F: FnMut(&StructGraph, NodeIndex, EdgeIndex), // Closure that can mutate environment
{
    let mut queue = VecDeque::new(); // Queue for BFS traversal
    let mut visited = IndexSet::new(); // Track visited nodes

    queue.push_back(start); // Start from the initial node
    visited.insert(start); // Mark the starting node as visited

    while let Some(node) = queue.pop_front() {
        // Iterate over neighbors of the current node
        for neighbor in graph.neighbors(node) {
            if !visited.contains(&neighbor) {
                // Find the edge between the current node and the neighbor
                if let Some(edge) = graph.find_edge(node, neighbor) {
                    if &graph[edge] == edge_type {
                        // Call the mutable closure with the graph, current node, and edge
                        node_action(graph, node, edge);

                        // Push the neighbor to the queue and mark as visited
                        queue.push_back(neighbor);
                        visited.insert(neighbor);
                    }
                }
            }
        }
    }

    visited // Return the visited nodes
}

pub fn traverse_by_edge_mut<'a, F>(
    graph: &'a mut StructGraph,    // Mutable reference to the graph
    edge_type: &'a StructRelation, // Edge type to filter on
    start_node: NodeIndex,         // Starting node
    mut node_action: F,            // Closure to mutate nodes and edges
) -> IndexSet<NodeIndex>
// Return the visited nodes
where
    F: FnMut(&mut StructGraph, NodeIndex, EdgeIndex), // Mutating closure
{
    let mut stack = VecDeque::new(); // Stack to keep track of nodes to visit
    let mut visited = IndexSet::new(); // Set to track visited nodes
    stack.push_back(start_node); // Start from the initial node

    while let Some(node) = stack.pop_back() {
        // Mark the current node as visited
        if visited.insert(node) {
            // Insert returns false if already visited
            // Step 1: Collect neighbors and edges that match the edge type
            let neighbors: Vec<(NodeIndex, EdgeIndex)> = graph
                .neighbors(node)
                .filter_map(|neighbor| {
                    // Find the edge between the current node and the neighbor
                    if let Some(edge) = graph.find_edge(node, neighbor) {
                        if &graph[edge] == edge_type {
                            // Collect the neighbor and edge if it matches the edge type
                            return Some((neighbor, edge));
                        }
                    }
                    None
                })
                .collect(); // Collect them into a vector

            // Step 2: Apply mutations
            for (neighbor, edge) in neighbors {
                // Mutate the current node and edge
                node_action(graph, node, edge);
                // Push the neighbor onto the stack for later visiting
                stack.push_back(neighbor);
            }
        }
    }
    visited // Return the visited nodes
}
