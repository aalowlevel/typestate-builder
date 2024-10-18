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
    res.push(builder::run(graph, map));

    // if let Some(builder_states) = builder_states::run(&graph, &map) {
    //     res.extend(builder_states);
    // }
    // if let Some(builder_build_impl) = builder_build_impl::run(&graph, &map) {
    //     res.push(builder_build_impl);
    // }
    res
}

mod builder {
    use std::rc::Rc;

    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::{
        mapkey, msg, traverse, StructElement, StructGraph, StructRelation, StructType,
    };

    pub(super) fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> TokenStream2 {
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

        let data = map.get(mapkey::startp::BUILDER_FIELD).map(|start| {
            enum IdentType {
                Field(Rc<syn::Ident>),
                Generic(Rc<syn::Ident>),
            }
            let action = |graph: &StructGraph, _edge, builder_node| match &graph[builder_node] {
                StructElement::BuilderField(ident) => IdentType::Field(Rc::clone(ident)),
                StructElement::BuilderGeneric(ident) => IdentType::Generic(Rc::clone(ident)),
                _ => panic!("Node cannot be other than builder field or generic."),
            };

            /* ⚠️ WARNING #WR04864549 Order-sensitive graph traversal function call. */
            let idents = traverse(
                graph,
                Some(&[
                    &StructRelation::BuilderFieldToBuilderGeneric,
                    &StructRelation::BuilderFieldTrain,
                ]),
                *start,
                true,
                action,
            );

            /* ✅ #TD04832691 Reduction. */
            let mut fields = Vec::with_capacity(idents.len());
            let mut generics = Vec::with_capacity(idents.len());
            for ident in idents {
                match ident {
                    IdentType::Field(rc) => fields.push(rc),
                    IdentType::Generic(rc) => generics.push(rc),
                }
            }
            (fields, generics)
        });

        match (ty, data) {
            (StructType::Named, data) => {
                if let Some((fields, generics)) = data {
                    let fields_ts = fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = &generics[i];
                            quote! { #field: #ty }
                        })
                        .collect::<Vec<_>>();
                    quote! {
                        #vis struct #ident <#(#generics),*> {
                            #(#fields_ts),*
                        }
                    }
                } else {
                    quote! {
                        #vis struct #ident {}
                    }
                }
            }
            (StructType::Unnamed, data) => {
                if let Some((_, generics)) = data {
                    quote! {
                        #vis struct #ident <#(#generics),*> (#(#generics),*);
                    }
                } else {
                    quote! {
                        #vis struct #ident ();
                    }
                }
            }
            (StructType::Unit, _) => {
                quote! {
                    #vis struct #ident;
                }
            }
        }
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
