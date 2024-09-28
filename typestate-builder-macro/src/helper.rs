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

use syn::{Ident, PathArguments, Type};

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
