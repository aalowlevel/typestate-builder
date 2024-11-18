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

use std::{
    fs::{self, File},
    io::Write,
};

use petgraph::{dot::Dot, Graph};
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

pub fn write_graph_to_file<N: std::fmt::Debug, E: std::fmt::Debug>(
    graph: &Graph<N, E>,
    filename: &str,
) -> std::io::Result<()> {
    let dot = format!("{:?}", Dot::new(graph));
    let mut path = std::env::current_dir()?;
    path.push("dots");
    let _ = fs::create_dir_all(&path);
    path.push(filename);
    let mut file = File::create(path)?;
    file.write_all(dot.as_bytes())?;
    Ok(())
}

pub mod string {
    use rand::Rng;

    pub fn to_titlecase(string: &str) -> String {
        string
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

    const LOWERCASE: &[char] = &[
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    ];
    pub fn rand_lowercase(len: usize, buffer: &mut String) {
        let mut rng = rand::thread_rng();
        let pool_len = LOWERCASE.len();
        for _ in 0..len {
            buffer.push(LOWERCASE[rng.gen_range(0..pool_len)]);
        }
    }
}
