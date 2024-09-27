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

use std::collections::{HashMap, HashSet};

use petgraph::{graph::NodeIndex, visit::Dfs, Graph};
use proc_macro_error::emit_call_site_warning;
use syn::{Expr, GenericArgument, Ident, Lifetime, PathArguments, Type};

use crate::{
    graph::{StructElement, StructRelation},
    write_graph_to_file,
};

pub fn run(graph: Graph<StructElement, StructRelation>, map: HashMap<String, NodeIndex>) {
    let graph = bind_field_generics(graph, &map);
    write_graph_to_file(&graph, "example.dot").unwrap();
}

fn bind_field_generics(
    mut graph: Graph<StructElement, StructRelation>,
    map: &HashMap<String, NodeIndex>,
) -> Graph<StructElement, StructRelation> {
    let dfs_fields = map.get("Field0").map(|f| Dfs::new(&graph, *f));
    if let Some(mut dfs_fields) = dfs_fields {
        while let Some(ix_field) = dfs_fields.next(&graph) {
            let Some(StructElement::Field(field)) = graph.node_weight_mut(ix_field) else {
                panic!("Only Field is accepted.");
            };

            // List some field assets recursively.
            let field_elements = FieldElements::default().list(&mut field.ty);
            emit_call_site_warning!(format!("{:#?}", field_elements));

            // Search for field assets in the main generics and match.
            let dfs_generics = map.get("Generic0").map(|f| Dfs::new(&graph, *f));

            // Search for field assets in where predicates.
        }
    }
    graph
}

#[derive(Debug, Default)]
struct FieldElements<'a> {
    idents: HashSet<&'a Ident>,
    lifetimes: HashSet<&'a Lifetime>,
    const_params: HashSet<&'a Ident>,
}

impl<'a> FieldElements<'a> {
    fn list(mut self, ty: &'a mut Type) -> Self {
        match ty {
            Type::Array(type_array) => {
                self = self.list(&mut type_array.elem);
                // Handle const generic in array length
                if let Expr::Path(expr_path) = &type_array.len {
                    if let Some(ident) = expr_path.path.get_ident() {
                        self.const_params.insert(ident);
                    } else {
                        // Handle multi-segment paths
                        for segment in &expr_path.path.segments {
                            self.idents.insert(&segment.ident);
                        }
                    }
                }
                self
            }
            Type::BareFn(type_bare_fn) => {
                for input in &mut type_bare_fn.inputs {
                    self = self.list(&mut input.ty);
                }
                if let syn::ReturnType::Type(_, return_type) = &mut type_bare_fn.output {
                    self = self.list(return_type);
                }
                self
            }
            Type::Group(type_group) => self.list(&mut type_group.elem),
            Type::ImplTrait(type_impl_trait) => {
                for bound in &mut type_impl_trait.bounds {
                    match bound {
                        syn::TypeParamBound::Trait(trait_bound) => {
                            for segment in &mut trait_bound.path.segments {
                                self.idents.insert(&segment.ident);
                                if let syn::PathArguments::AngleBracketed(args) =
                                    &mut segment.arguments
                                {
                                    for arg in &mut args.args {
                                        match arg {
                                            syn::GenericArgument::Type(ty) => self = self.list(ty),
                                            syn::GenericArgument::Lifetime(lt) => {
                                                self.lifetimes.insert(lt);
                                            }
                                            syn::GenericArgument::Const(Expr::Path(expr_path)) => {
                                                if let Some(ident) = expr_path.path.get_ident() {
                                                    self.const_params.insert(ident);
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                        syn::TypeParamBound::Lifetime(lt) => {
                            self.lifetimes.insert(lt);
                        }
                        syn::TypeParamBound::Verbatim(_) => {}
                        _ => {}
                    }
                }
                self
            }
            Type::Infer(_) => self,
            Type::Macro(type_macro) => {
                if let Some(ident) = type_macro.mac.path.get_ident() {
                    self.idents.insert(ident);
                }
                self
            }
            Type::Never(_) => self,
            Type::Paren(type_paren) => self.list(&mut type_paren.elem),

            Type::Path(type_path) => {
                for segment in &mut type_path.path.segments {
                    self.idents.insert(&segment.ident);
                    match &mut segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            for arg in &mut args.args {
                                match arg {
                                    GenericArgument::Type(ty) => self = self.list(ty),
                                    GenericArgument::Lifetime(lt) => {
                                        self.lifetimes.insert(lt);
                                    }
                                    GenericArgument::Const(Expr::Path(expr_path)) => {
                                        if let Some(ident) = expr_path.path.get_ident() {
                                            self.const_params.insert(ident);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        PathArguments::Parenthesized(args) => {
                            for ty in &mut args.inputs {
                                self = self.list(ty);
                            }
                            if let syn::ReturnType::Type(_, ty) = &mut args.output {
                                self = self.list(ty);
                            }
                        }
                        PathArguments::None => {}
                    }
                }
                self
            }
            Type::Ptr(type_ptr) => self.list(&mut type_ptr.elem),
            Type::Reference(type_reference) => {
                if let Some(lt) = &type_reference.lifetime {
                    self.lifetimes.insert(lt);
                }
                self.list(&mut type_reference.elem)
            }
            Type::Slice(type_slice) => self.list(&mut type_slice.elem),
            Type::TraitObject(type_trait_object) => {
                for bound in &mut type_trait_object.bounds {
                    match bound {
                        syn::TypeParamBound::Trait(trait_bound) => {
                            for segment in &mut trait_bound.path.segments {
                                self.idents.insert(&segment.ident);
                                if let syn::PathArguments::AngleBracketed(args) =
                                    &mut segment.arguments
                                {
                                    for arg in &mut args.args {
                                        match arg {
                                            syn::GenericArgument::Type(ty) => self = self.list(ty),
                                            syn::GenericArgument::Lifetime(lt) => {
                                                self.lifetimes.insert(lt);
                                            }
                                            syn::GenericArgument::Const(Expr::Path(expr_path)) => {
                                                if let Some(ident) = expr_path.path.get_ident() {
                                                    self.const_params.insert(ident);
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                        syn::TypeParamBound::Lifetime(lt) => {
                            self.lifetimes.insert(lt);
                        }
                        syn::TypeParamBound::Verbatim(_) => {}
                        _ => {}
                    }
                }
                self
            }
            Type::Tuple(type_tuple) => {
                for elem in &mut type_tuple.elems {
                    self = self.list(elem);
                }
                self
            }
            Type::Verbatim(_) => self,
            _ => self,
        }
    }

    fn match_generics_main(
        &self,
        graph: &mut Graph<StructElement, StructRelation>,
        map: &HashMap<String, NodeIndex>,
    ) {
    }
}
