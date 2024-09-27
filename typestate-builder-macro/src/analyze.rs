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
use syn::Type;

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

            // list all field types recursively.
            let mut types = Vec::new();
            types = list_types(&field.ty, types);
            emit_call_site_warning!(format!("{:?}", types))
        }
    }
    graph
}
fn list_types<'a>(ty: &'a Type, mut types: Vec<&'a Type>) -> Vec<&'a Type> {
    types.push(ty);
    match ty {
        syn::Type::Array(type_array) => list_types(&type_array.elem, types),
        syn::Type::BareFn(type_bare_fn) => {
            for input in &type_bare_fn.inputs {
                types = list_types(&input.ty, types);
            }
            if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                types = list_types(return_type, types);
            }
            types
        }
        syn::Type::Group(type_group) => list_types(&type_group.elem, types),
        syn::Type::ImplTrait(type_impl_trait) => {
            for bound in &type_impl_trait.bounds {
                if let syn::TypeParamBound::Trait(trait_bound) = bound {
                    if let Some(last_segment) = trait_bound.path.segments.last() {
                        if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
                            for arg in &args.args {
                                if let syn::GenericArgument::Type(ty) = arg {
                                    types = list_types(ty, types);
                                }
                            }
                        }
                    }
                }
            }
            types
        }
        syn::Type::Infer(_) => types,
        syn::Type::Macro(_) => types,
        syn::Type::Never(_) => types,
        syn::Type::Paren(type_paren) => list_types(&type_paren.elem, types),
        syn::Type::Path(type_path) => {
            if let Some(last_segment) = type_path.path.segments.last() {
                if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(ty) = arg {
                            types = list_types(ty, types);
                        }
                    }
                }
            }
            types
        }
        syn::Type::Ptr(type_ptr) => list_types(&type_ptr.elem, types),
        syn::Type::Reference(type_reference) => list_types(&type_reference.elem, types),
        syn::Type::Slice(type_slice) => list_types(&type_slice.elem, types),
        syn::Type::TraitObject(type_trait_object) => {
            for bound in &type_trait_object.bounds {
                if let syn::TypeParamBound::Trait(trait_bound) = bound {
                    if let Some(last_segment) = trait_bound.path.segments.last() {
                        if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
                            for arg in &args.args {
                                if let syn::GenericArgument::Type(ty) = arg {
                                    types = list_types(ty, types);
                                }
                            }
                        }
                    }
                }
            }
            types
        }
        syn::Type::Tuple(type_tuple) => {
            for elem in &type_tuple.elems {
                types = list_types(elem, types);
            }
            types
        }
        syn::Type::Verbatim(_) => types,
        _ => types,
    }
}
