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
use syn::{Expr, Ident, Lifetime, Type};

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
            let idents = Vec::new();
            let lifetimes = Vec::new();
            let const_params = Vec::new();
            let (idents, lifetimes, const_params) =
                list_idents(&field.ty, idents, lifetimes, const_params);
            emit_call_site_warning!(format!("{:#?}{:#?}{:#?}", idents, lifetimes, const_params));
            // for ty in idents {
            //     emit_call_site_warning!(syn_element_to_string(ty));
            // }
        }
    }
    graph
}

fn list_idents<'a>(
    ty: &'a Type,
    mut idents: Vec<&'a Ident>,
    mut lifetimes: Vec<&'a Lifetime>,
    mut const_params: Vec<&'a Expr>,
) -> (Vec<&'a Ident>, Vec<&'a Lifetime>, Vec<&'a Expr>) {
    match ty {
        syn::Type::Array(type_array) => {
            if let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Int(_int_lit),
                ..
            }) = &type_array.len
            {
                const_params.push(&type_array.len);
            }
            let (new_idents, new_lifetimes, new_const_params) =
                list_idents(&type_array.elem, idents, lifetimes, const_params);
            idents = new_idents;
            lifetimes = new_lifetimes;
            const_params = new_const_params;
        }
        syn::Type::BareFn(type_bare_fn) => {
            for input in &type_bare_fn.inputs {
                let (new_idents, new_lifetimes, new_const_params) =
                    list_idents(&input.ty, idents, lifetimes, const_params);
                idents = new_idents;
                lifetimes = new_lifetimes;
                const_params = new_const_params;
            }
            if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                let (new_idents, new_lifetimes, new_const_params) =
                    list_idents(return_type, idents, lifetimes, const_params);
                idents = new_idents;
                lifetimes = new_lifetimes;
                const_params = new_const_params;
            }
        }
        syn::Type::Group(type_group) => {
            let (new_idents, new_lifetimes, new_const_params) =
                list_idents(&type_group.elem, idents, lifetimes, const_params);
            idents = new_idents;
            lifetimes = new_lifetimes;
            const_params = new_const_params;
        }
        syn::Type::ImplTrait(type_impl_trait) => {
            for bound in &type_impl_trait.bounds {
                match bound {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        idents.extend(trait_bound.path.get_ident());
                        for segment in &trait_bound.path.segments {
                            idents.push(&segment.ident);
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                                for arg in &args.args {
                                    match arg {
                                        syn::GenericArgument::Type(ty) => {
                                            let (new_idents, new_lifetimes, new_const_params) =
                                                list_idents(ty, idents, lifetimes, const_params);
                                            idents = new_idents;
                                            lifetimes = new_lifetimes;
                                            const_params = new_const_params;
                                        }
                                        syn::GenericArgument::Lifetime(lt) => lifetimes.push(lt),
                                        syn::GenericArgument::Const(expr) => {
                                            const_params.push(expr)
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    syn::TypeParamBound::Lifetime(lt) => lifetimes.push(lt),
                    syn::TypeParamBound::Verbatim(_token_stream) => {}
                    _ => {}
                }
            }
        }
        syn::Type::Infer(_) => {}
        syn::Type::Macro(type_macro) => {
            if let Some(ident) = type_macro.mac.path.get_ident() {
                idents.push(ident);
            }
        }
        syn::Type::Never(_) => {}
        syn::Type::Paren(type_paren) => {
            let (new_idents, new_lifetimes, new_const_params) =
                list_idents(&type_paren.elem, idents, lifetimes, const_params);
            idents = new_idents;
            lifetimes = new_lifetimes;
            const_params = new_const_params;
        }
        syn::Type::Path(type_path) => {
            if let Some(qself) = &type_path.qself {
                let (new_idents, new_lifetimes, new_const_params) =
                    list_idents(&qself.ty, idents, lifetimes, const_params);
                idents = new_idents;
                lifetimes = new_lifetimes;
                const_params = new_const_params;
            }
            for segment in &type_path.path.segments {
                idents.push(&segment.ident);
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        match arg {
                            syn::GenericArgument::Type(ty) => {
                                let (new_idents, new_lifetimes, new_const_params) =
                                    list_idents(ty, idents, lifetimes, const_params);
                                idents = new_idents;
                                lifetimes = new_lifetimes;
                                const_params = new_const_params;
                            }
                            syn::GenericArgument::Lifetime(lt) => lifetimes.push(lt),
                            syn::GenericArgument::Const(expr) => const_params.push(expr),
                            _ => {}
                        }
                    }
                }
            }
        }
        syn::Type::Ptr(type_ptr) => {
            let (new_idents, new_lifetimes, new_const_params) =
                list_idents(&type_ptr.elem, idents, lifetimes, const_params);
            idents = new_idents;
            lifetimes = new_lifetimes;
            const_params = new_const_params;
        }
        syn::Type::Reference(type_reference) => {
            if let Some(lt) = &type_reference.lifetime {
                lifetimes.push(lt);
            }
            let (new_idents, new_lifetimes, new_const_params) =
                list_idents(&type_reference.elem, idents, lifetimes, const_params);
            idents = new_idents;
            lifetimes = new_lifetimes;
            const_params = new_const_params;
        }
        syn::Type::Slice(type_slice) => {
            let (new_idents, new_lifetimes, new_const_params) =
                list_idents(&type_slice.elem, idents, lifetimes, const_params);
            idents = new_idents;
            lifetimes = new_lifetimes;
            const_params = new_const_params;
        }
        syn::Type::TraitObject(type_trait_object) => {
            for bound in &type_trait_object.bounds {
                match bound {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        idents.extend(trait_bound.path.get_ident());
                        for segment in &trait_bound.path.segments {
                            idents.push(&segment.ident);
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                                for arg in &args.args {
                                    match arg {
                                        syn::GenericArgument::Type(ty) => {
                                            let (new_idents, new_lifetimes, new_const_params) =
                                                list_idents(ty, idents, lifetimes, const_params);
                                            idents = new_idents;
                                            lifetimes = new_lifetimes;
                                            const_params = new_const_params;
                                        }
                                        syn::GenericArgument::Lifetime(lt) => lifetimes.push(lt),
                                        syn::GenericArgument::Const(expr) => {
                                            const_params.push(expr)
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    syn::TypeParamBound::Lifetime(lt) => lifetimes.push(lt),
                    syn::TypeParamBound::Verbatim(_token_stream) => {}
                    _ => {}
                }
            }
        }
        syn::Type::Tuple(type_tuple) => {
            for elem in &type_tuple.elems {
                let (new_idents, new_lifetimes, new_const_params) =
                    list_idents(elem, idents, lifetimes, const_params);
                idents = new_idents;
                lifetimes = new_lifetimes;
                const_params = new_const_params;
            }
        }
        syn::Type::Verbatim(_) => {}
        _ => {}
    }
    (idents, lifetimes, const_params)
}
