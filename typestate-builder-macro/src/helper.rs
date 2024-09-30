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

use std::{fs::File, io::Write};

use petgraph::{dot::Dot, Graph};
use syn::{Ident, Path, PathArguments, Type, TypeParam, TypePath};

pub fn extract_ident(ty: &Type) -> Option<&Ident> {
    match ty {
        Type::Array(_) => None,
        Type::BareFn(_) => None,
        Type::Group(type_group) => extract_ident(&type_group.elem),
        Type::ImplTrait(_) => None,
        Type::Infer(_) => None,
        Type::Macro(_) => None,
        Type::Never(_) => None,
        Type::Paren(type_paren) => extract_ident(&type_paren.elem),
        Type::Path(type_path) => {
            type_path
                .path
                .segments
                .last()
                .and_then(|seg| match &seg.arguments {
                    PathArguments::None => Some(&seg.ident),
                    _ => None,
                })
        }
        Type::Ptr(_) => None,
        Type::Reference(type_reference) => extract_ident(&type_reference.elem),
        Type::Slice(_) => None,
        Type::TraitObject(_) => None,
        Type::Tuple(_) => None,
        Type::Verbatim(_) => None,
        _ => None, // This catches any new variants added in the future
    }
}

pub fn write_graph_to_file<N: std::fmt::Debug, E: std::fmt::Debug>(
    graph: &Graph<N, E>,
    filename: &str,
) -> std::io::Result<()> {
    let dot = format!("{:?}", Dot::new(graph));
    let mut file = File::create(filename)?;
    file.write_all(dot.as_bytes())?;
    Ok(())
}

pub fn ident_to_titlecase(syn: &syn::Ident) -> String {
    let node_name = syn.to_string();
    node_name
        .split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().chain(chars).collect(),
            }
        })
        .collect()
}

pub fn type_equals_type_param(ty: &Type, param: &TypeParam) -> bool {
    match ty {
        Type::Path(TypePath { path, .. }) => path_equals_type_param(path, param),
        _ => false, // Other variants of Type are not considered equal to TypeParam
    }
}

fn path_equals_type_param(path: &Path, param: &TypeParam) -> bool {
    if path.segments.len() != 1 {
        return false;
    }

    let segment = &path.segments[0];

    // Compare the identifier of the path segment with the identifier of the TypeParam
    segment.ident == param.ident && segment.arguments.is_empty()
}
