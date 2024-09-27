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
use syn::{Ident, Type};

use crate::{
    graph::{StructElement, StructRelation},
    syn_element_to_string, write_graph_to_file,
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

            // list all field types recursively.
            let mut idents = Vec::new();
            idents = list_idents(&field.ty, idents);
            emit_call_site_warning!(format!("{:?}", idents));
            for ty in idents {
                emit_call_site_warning!(syn_element_to_string(ty));
            }
        }
    }
    graph
}

fn list_idents<'a>(ty: &'a Type, mut idents: Vec<&'a Ident>) -> Vec<&'a Ident> {
    match ty {
        syn::Type::Array(type_array) => list_idents(&type_array.elem, idents),
        syn::Type::BareFn(type_bare_fn) => {
            for input in &type_bare_fn.inputs {
                idents = list_idents(&input.ty, idents);
            }
            if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                idents = list_idents(return_type, idents);
            }
            idents
        }
        syn::Type::Group(type_group) => list_idents(&type_group.elem, idents),
        syn::Type::ImplTrait(type_impl_trait) => {
            for bound in &type_impl_trait.bounds {
                if let syn::TypeParamBound::Trait(trait_bound) = bound {
                    idents.extend(trait_bound.path.get_ident());
                    for segment in &trait_bound.path.segments {
                        idents.push(&segment.ident);
                        if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                            for arg in &args.args {
                                if let syn::GenericArgument::Type(ty) = arg {
                                    idents = list_idents(ty, idents);
                                }
                            }
                        }
                    }
                }
            }
            idents
        }
        syn::Type::Infer(_) => idents,
        syn::Type::Macro(type_macro) => {
            if let Some(ident) = type_macro.mac.path.get_ident() {
                idents.push(ident);
            }
            idents
        }
        syn::Type::Never(_) => idents,
        syn::Type::Paren(type_paren) => list_idents(&type_paren.elem, idents),
        syn::Type::Path(type_path) => {
            if let Some(ident) = type_path.path.get_ident() {
                idents.push(ident);
            }
            for segment in &type_path.path.segments {
                idents.push(&segment.ident);
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(ty) = arg {
                            idents = list_idents(ty, idents);
                        }
                    }
                }
            }
            idents
        }
        syn::Type::Ptr(type_ptr) => list_idents(&type_ptr.elem, idents),
        syn::Type::Reference(type_reference) => list_idents(&type_reference.elem, idents),
        syn::Type::Slice(type_slice) => list_idents(&type_slice.elem, idents),
        syn::Type::TraitObject(type_trait_object) => {
            for bound in &type_trait_object.bounds {
                if let syn::TypeParamBound::Trait(trait_bound) = bound {
                    idents.extend(trait_bound.path.get_ident());
                    for segment in &trait_bound.path.segments {
                        idents.push(&segment.ident);
                        if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                            for arg in &args.args {
                                if let syn::GenericArgument::Type(ty) = arg {
                                    idents = list_idents(ty, idents);
                                }
                            }
                        }
                    }
                }
            }
            idents
        }
        syn::Type::Tuple(type_tuple) => {
            for elem in &type_tuple.elems {
                idents = list_idents(elem, idents);
            }
            idents
        }
        syn::Type::Verbatim(_) => idents,
        _ => idents,
    }
}
