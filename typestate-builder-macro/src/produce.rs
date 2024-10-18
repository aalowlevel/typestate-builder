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

use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::TokenStream as TokenStream2;

use crate::graph::StructGraph;

pub fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Vec<TokenStream2> {
    let mut res = Vec::new();
    if let Some(builder) = builder::run(graph, map) {
        res.push(builder);
    }
    // if let Some(builder_states) = builder_states::run(&graph, &map) {
    //     res.extend(builder_states);
    // }
    // if let Some(builder_build_impl) = builder_build_impl::run(&graph, &map) {
    //     res.push(builder_build_impl);
    // }
    res
}

mod builder {
    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::{mapkey, msg, StructElement, StructGraph, StructType};

    pub(super) fn run(
        graph: &StructGraph,
        map: &IndexMap<String, NodeIndex>,
    ) -> Option<TokenStream2> {
        let Some(ix) = map.get(mapkey::uniq::BUILDER_IDENT) else {
            panic!("{}", msg::ix::BUILDER_IDENT);
        };
        let StructElement::BuilderIdent(ident) = &graph[*ix] else {
            panic!("{}", msg::node::IDENT);
        };
        let Some(ix) = map.get(mapkey::uniq::VIS) else {
            panic!("{}", msg::ix::VIS);
        };
        let StructElement::Visibility(vis) = &graph[*ix] else {
            panic!("{}", msg::node::VIS);
        };
        let Some(ix) = map.get(mapkey::uniq::TYPE) else {
            panic!("{}", msg::ix::TYPE);
        };
        let StructElement::Type(ty) = &graph[*ix] else {
            panic!("{}", msg::node::TYPE);
        };

        Some(match ty {
            StructType::Named => {
                quote! {
                    #vis struct #ident
                }
            }
            StructType::Unnamed => {
                quote! {}
            }
            StructType::Unit => {
                quote! {}
            }
        })
    }
}

mod builder_states {
    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::StructGraph;

    pub(super) fn run(
        graph: &StructGraph,
        map: &IndexMap<String, NodeIndex>,
    ) -> Option<TokenStream2> {
        Some(quote! {})
    }
}

mod builder_build_impl {
    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::StructGraph;

    pub(super) fn run(
        graph: &StructGraph,
        map: &IndexMap<String, NodeIndex>,
    ) -> Option<TokenStream2> {
        Some(quote! {})
    }
}

mod builder_impl {
    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::StructGraph;

    pub(super) fn run(
        graph: &StructGraph,
        map: &IndexMap<String, NodeIndex>,
    ) -> Option<TokenStream2> {
        Some(quote! {})
    }
}

mod builder_new_impl {
    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::StructGraph;

    pub(super) fn run(
        graph: &StructGraph,
        map: &IndexMap<String, NodeIndex>,
    ) -> Option<TokenStream2> {
        Some(quote! {})
    }
}
