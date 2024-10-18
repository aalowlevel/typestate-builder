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
    vec![
        builder::run(graph, map),
        builder_states::run(graph, map),
        builder_new_impl::run(graph, map),
    ]
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
            panic!("{}", msg::node::BUILDER_IDENT);
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
        }
    }
}

mod builder_states {
    use std::rc::Rc;

    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::{
        mapkey, msg, traverse, BuilderStateAdded, StructElement, StructGraph, StructRelation,
    };

    pub(super) fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> TokenStream2 {
        let Some(ix) = map.get(mapkey::uniq::VIS) else {
            panic!("{}", msg::ix::VIS);
        };
        let StructElement::Visibility(vis) = &graph[*ix] else {
            panic!("{}", msg::node::VIS);
        };

        if let Some(start) = map.get(mapkey::startp::BUILDER_FIELD) {
            enum PairType {
                Empty(Rc<syn::Ident>),
                Added(Rc<BuilderStateAdded>),
                None,
            }
            let action = |graph: &StructGraph, _edge, builder_node| match &graph[builder_node] {
                StructElement::BuilderStateEmpty(ident) => PairType::Empty(Rc::clone(ident)),
                StructElement::BuilderStateAdded(builder_state_added) => {
                    PairType::Added(Rc::clone(builder_state_added))
                }
                _ => PairType::None,
            };

            /* ⚠️ WARNING #WR23330504 Order-sensitive graph traversal function call. */
            let pairs = traverse(
                graph,
                Some(&[
                    &StructRelation::BuilderStatePair,
                    &StructRelation::BuilderFieldToBuilderState,
                    &StructRelation::BuilderFieldTrain,
                ]),
                *start,
                false,
                action,
            );

            let pairs = pairs
                .into_iter()
                .map(|pair| match pair {
                    PairType::Empty(ident) => quote! { #vis struct #ident; },
                    PairType::Added(bsa) => {
                        let ident = &bsa.ident;
                        let generics = if !bsa.generics.is_empty() {
                            let generics = &bsa.generics;
                            quote! { <#(#generics),*> }
                        } else {
                            quote! {}
                        };
                        let ty = &bsa.ty;
                        let phantoms = if !bsa.phantoms.is_empty() {
                            let phantoms = &bsa.phantoms;
                            quote! { , #(#phantoms),* }
                        } else {
                            quote! {}
                        };
                        let where_clause = if !bsa.where_predicates.is_empty() {
                            let wps = &bsa.where_predicates;
                            quote! { where #(#wps),* }
                        } else {
                            quote! {}
                        };

                        quote! {
                            #vis struct #ident #generics(#ty #phantoms) #where_clause;
                        }
                    }
                    PairType::None => quote! {},
                })
                .collect::<Vec<_>>();
            quote! { #(#pairs)* }
        } else {
            quote! {}
        }
    }
}

mod builder_new_impl {
    use std::rc::Rc;

    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::{
        mapkey, msg, traverse, StructElement, StructGraph, StructRelation, StructType,
    };

    pub(super) fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> TokenStream2 {
        let Some(ix) = map.get(mapkey::uniq::IDENT) else {
            panic!("{}", msg::ix::IDENT);
        };
        let StructElement::Ident(ident) = &graph[*ix] else {
            panic!("{}", msg::node::IDENT);
        };
        let Some(ix) = map.get(mapkey::uniq::BUILDER_IDENT) else {
            panic!("{}", msg::ix::BUILDER_IDENT);
        };
        let StructElement::BuilderIdent(builder_ident) = &graph[*ix] else {
            panic!("{}", msg::node::BUILDER_IDENT);
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

        let generics = map.get(mapkey::startp::GENERICS).map(|start| {
            let generics = traverse(
                graph,
                Some(&[&StructRelation::GenericTrain]),
                *start,
                true,
                |graph, _edge, node_generic| {
                    let StructElement::Generic(generic) = &graph[node_generic] else {
                        panic!("{}", msg::node::GENERIC);
                    };
                    Rc::clone(&generic.syn)
                },
            );
            if !generics.is_empty() {
                /* 🌀 COMPLEXITY #CP27010317 Using syn::GenericParam for the first declaration is OK.
                However, There are some examples for the second part usage:
                - Ident of const generics must be used. For example: impl<const N: usize> Foo<N>.
                - Associated Item Constraints are not allowed here. For example: impl<T: Action> Dispatcher<T>
                This means that both the first and second parts must be differentiated. */
                let mut first = Vec::with_capacity(generics.len());
                let mut second = Vec::with_capacity(generics.len());
                for generic in generics {
                    first.push(quote! { #generic });
                    match generic.as_ref() {
                        syn::GenericParam::Lifetime(lifetime_param) => {
                            let lt = &lifetime_param.lifetime;
                            second.push(quote! { #lt });
                        }
                        syn::GenericParam::Type(type_param) => {
                            let ident = &type_param.ident;
                            second.push(quote! { #ident });
                        }
                        syn::GenericParam::Const(const_param) => {
                            let ident = &const_param.ident;
                            second.push(quote! { #ident });
                        }
                    }
                }
                (quote! { <#(#first),*> }, quote! { <#(#second),*> })
            } else {
                (quote! {}, quote! {})
            }
        });
        let (first, second) = match generics {
            Some((first, second)) => (Some(first), Some(second)),
            None => (None, None),
        };

        let where_clause = map.get(mapkey::startp::WP).map(|start| {
            let wps = traverse(
                graph,
                Some(&[&StructRelation::WherePredicateTrain]),
                *start,
                true,
                |graph, _edge, node_wp| {
                    let StructElement::WherePredicate(wp) = &graph[node_wp] else {
                        panic!("{}", msg::node::WP);
                    };
                    Rc::clone(&wp.syn)
                },
            );
            if !wps.is_empty() {
                quote! { where #(#wps),* }
            } else {
                quote! {}
            }
        });

        let builder_others = map.get(mapkey::startp::BUILDER_FIELD).map(|start| {
            let collected = traverse(
                graph,
                Some(&[
                    &StructRelation::BuilderFieldToBuilderState,
                    &StructRelation::BuilderFieldTrain,
                ]),
                *start,
                true,
                |graph, _edge, node| match &graph[node] {
                    StructElement::BuilderStateEmpty(rc) => (None, Some(Rc::clone(rc))),
                    StructElement::BuilderField(rc) => (Some(Rc::clone(rc)), None),
                    _ => (None, None),
                },
            );
            let mut fields = Vec::with_capacity(collected.len());
            let mut empties = Vec::with_capacity(collected.len());
            for (field, empty) in collected {
                if let Some(inner) = field {
                    fields.push(inner);
                }
                if let Some(inner) = empty {
                    empties.push(inner);
                }
            }
            assert_eq!(fields.len(), empties.len());

            let return_type_generics = if !empties.is_empty() {
                quote! { <#(#empties),*> }
            } else {
                quote! {}
            };
            let constructor_data = match ty {
                StructType::Named => {
                    let constructor_fields = fields
                        .into_iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = &empties[i];
                            quote! { #field: #ty }
                        })
                        .collect::<Vec<_>>();
                    quote! {{ #(#constructor_fields),* }}
                }
                StructType::Unnamed => {
                    quote! {( #(#empties),* )}
                }
            };
            (return_type_generics, constructor_data)
        });

        match builder_others {
            Some((return_type_generics, constructor_data)) => {
                quote! {
                    impl #first #ident #second #where_clause {
                        #vis fn builder() -> #builder_ident #return_type_generics  {
                            #builder_ident #constructor_data
                        }
                    }
                }
            }
            None => quote! {},
        }
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
