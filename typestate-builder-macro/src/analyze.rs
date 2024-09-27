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

use std::collections::HashMap;

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
    graph: Graph<StructElement, StructRelation>,
    map: &HashMap<String, NodeIndex>,
) -> Graph<StructElement, StructRelation> {
    let dfs = map.get("Field0").map(|f| Dfs::new(&graph, *f));
    if let Some(mut dfs) = dfs {
        while let Some(ix) = dfs.next(&graph) {
            let StructElement::Field(field) = &graph[ix] else {
                panic!("Only Field is accepted.");
            };

            // list some field assets recursively.
            let field_elements = FieldElements::default().list(&field.ty);
            emit_call_site_warning!(format!("{:#?}", field_elements));
            // for ty in idents {
            //     emit_call_site_warning!(syn_element_to_string(ty));
            // }
        }
    }
    graph
}

#[derive(Debug, Default)]
struct FieldElements<'a> {
    idents: Vec<&'a Ident>,
    lifetimes: Vec<&'a Lifetime>,
    const_params: Vec<&'a Ident>,
}

impl<'a> FieldElements<'a> {
    fn list(mut self, ty: &'a Type) -> Self {
        match ty {
            Type::Array(type_array) => {
                self = self.list(&type_array.elem);
                // Handle const generic in array length
                if let Expr::Path(expr_path) = &type_array.len {
                    if let Some(ident) = expr_path.path.get_ident() {
                        self.const_params.push(ident);
                    } else {
                        // Handle multi-segment paths
                        for segment in &expr_path.path.segments {
                            self.idents.push(&segment.ident);
                        }
                    }
                }
                self
            }
            Type::BareFn(type_bare_fn) => {
                for input in &type_bare_fn.inputs {
                    self = self.list(&input.ty);
                }
                if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                    self = self.list(return_type);
                }
                self
            }
            Type::Group(type_group) => self.list(&type_group.elem),
            Type::ImplTrait(type_impl_trait) => {
                for bound in &type_impl_trait.bounds {
                    match bound {
                        syn::TypeParamBound::Trait(trait_bound) => {
                            if let Some(ident) = trait_bound.path.get_ident() {
                                self.idents.push(ident);
                            }
                            for segment in &trait_bound.path.segments {
                                self.idents.push(&segment.ident);
                                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                                {
                                    for arg in &args.args {
                                        match arg {
                                            syn::GenericArgument::Type(ty) => self = self.list(ty),
                                            syn::GenericArgument::Lifetime(lt) => {
                                                self.lifetimes.push(lt)
                                            }
                                            syn::GenericArgument::Const(Expr::Path(expr_path)) => {
                                                if let Some(ident) = expr_path.path.get_ident() {
                                                    self.const_params.push(ident);
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                        syn::TypeParamBound::Lifetime(lt) => self.lifetimes.push(lt),
                        syn::TypeParamBound::Verbatim(_) => {}
                        _ => {}
                    }
                }
                self
            }
            Type::Infer(_) => self,
            Type::Macro(type_macro) => {
                if let Some(ident) = type_macro.mac.path.get_ident() {
                    self.idents.push(ident);
                }
                self
            }
            Type::Never(_) => self,
            Type::Paren(type_paren) => self.list(&type_paren.elem),

            Type::Path(type_path) => {
                for segment in &type_path.path.segments {
                    self.idents.push(&segment.ident);
                    match &segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            for arg in &args.args {
                                match arg {
                                    GenericArgument::Type(ty) => self = self.list(ty),
                                    GenericArgument::Lifetime(lt) => self.lifetimes.push(lt),
                                    GenericArgument::Const(Expr::Path(expr_path)) => {
                                        if let Some(ident) = expr_path.path.get_ident() {
                                            self.const_params.push(ident);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        PathArguments::Parenthesized(args) => {
                            for ty in &args.inputs {
                                self = self.list(ty);
                            }
                            if let syn::ReturnType::Type(_, ty) = &args.output {
                                self = self.list(ty);
                            }
                        }
                        PathArguments::None => {}
                    }
                }
                self
            }
            Type::Ptr(type_ptr) => self.list(&type_ptr.elem),
            Type::Reference(type_reference) => {
                if let Some(lt) = &type_reference.lifetime {
                    self.lifetimes.push(lt);
                }
                self.list(&type_reference.elem)
            }
            Type::Slice(type_slice) => self.list(&type_slice.elem),
            Type::TraitObject(type_trait_object) => {
                for bound in &type_trait_object.bounds {
                    match bound {
                        syn::TypeParamBound::Trait(trait_bound) => {
                            if let Some(ident) = trait_bound.path.get_ident() {
                                self.idents.push(ident);
                            }
                            for segment in &trait_bound.path.segments {
                                self.idents.push(&segment.ident);
                                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                                {
                                    for arg in &args.args {
                                        match arg {
                                            syn::GenericArgument::Type(ty) => self = self.list(ty),
                                            syn::GenericArgument::Lifetime(lt) => {
                                                self.lifetimes.push(lt)
                                            }
                                            syn::GenericArgument::Const(Expr::Path(expr_path)) => {
                                                if let Some(ident) = expr_path.path.get_ident() {
                                                    self.const_params.push(ident);
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                        syn::TypeParamBound::Lifetime(lt) => self.lifetimes.push(lt),
                        syn::TypeParamBound::Verbatim(_) => {}
                        _ => {}
                    }
                }
                self
            }
            Type::Tuple(type_tuple) => {
                for elem in &type_tuple.elems {
                    self = self.list(elem);
                }
                self
            }
            Type::Verbatim(_) => self,
            _ => self,
        }
    }
}
