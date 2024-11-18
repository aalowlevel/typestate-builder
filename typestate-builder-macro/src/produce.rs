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
        builder_impl::run(graph, map),
        builder_build_impl::run(graph, map),
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
            enum ActionResult {
                Field(Rc<syn::Ident>),
                Generic(Rc<syn::Ident>),
            }
            let action = |graph: &StructGraph, _edge, builder_node| match &graph[builder_node] {
                StructElement::BuilderField(ident) => ActionResult::Field(Rc::clone(ident)),
                StructElement::BuilderGeneric(ident) => ActionResult::Generic(Rc::clone(ident)),
                _ => panic!("Node cannot be other than builder field or generic."),
            };

            /* ⚠️ WARNING #WR04864549 Order-sensitive graph traversal function call. */
            let idents: Vec<_> = traverse(
                graph,
                &[
                    &StructRelation::BuilderFieldToBuilderGeneric,
                    &StructRelation::BuilderFieldTrain,
                ],
                *start,
                true,
                action,
            );

            /* ✅ #TD04832691 Reduction. */
            let mut fields = Vec::with_capacity(idents.len());
            let mut generics = Vec::with_capacity(idents.len());
            for ident in idents {
                match ident {
                    ActionResult::Field(rc) => fields.push(rc),
                    ActionResult::Generic(rc) => generics.push(rc),
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
    use indexmap::IndexMap;
    use petgraph::graph::NodeIndex;
    use proc_macro2::TokenStream as TokenStream2;
    use quote::quote;

    use crate::graph::{mapkey, msg, traverse, StructElement, StructGraph, StructRelation};

    pub(super) fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> TokenStream2 {
        let Some(ix) = map.get(mapkey::uniq::VIS) else {
            panic!("{}", msg::ix::VIS);
        };
        let StructElement::Visibility(vis) = &graph[*ix] else {
            panic!("{}", msg::node::VIS);
        };

        let action = |graph: &StructGraph, _edge, builder_node| match &graph[builder_node] {
            StructElement::BuilderStateEmpty(ident) => quote! { #vis struct #ident; },
            StructElement::BuilderStateAdded(bsa) => {
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
            _ => quote! {},
        };

        if let Some(pairs) = map
            .get(mapkey::startp::BUILDER_FIELD)
            .map(|start| -> Vec<_> {
                /* ⚠️ WARNING #WR23330504 Order-sensitive graph traversal function call. */
                traverse(
                    graph,
                    &[
                        &StructRelation::BuilderStatePair,
                        &StructRelation::BuilderFieldToBuilderState,
                        &StructRelation::BuilderFieldTrain,
                    ],
                    *start,
                    false,
                    action,
                )
            })
        {
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
        let Some(ix) = map.get(mapkey::uniq::METHOD_BUILDER_IDENT) else {
            panic!("{}", msg::ix::METHOD_BUILDER_IDENT);
        };
        let StructElement::MethodBuilderIdent(mb_ident) = &graph[*ix] else {
            panic!("{}", msg::node::METHOD_BUILDER_IDENT);
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
            let generics: Vec<_> = traverse(
                graph,
                &[&StructRelation::GenericTrain],
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
            let wps: Vec<_> = traverse(
                graph,
                &[&StructRelation::WherePredicateTrain],
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

        let collections = map.get(mapkey::startp::BUILDER_FIELD).map(|start| {
            let collected: Vec<_> = traverse(
                graph,
                &[
                    &StructRelation::BuilderFieldToBuilderState,
                    &StructRelation::BuilderFieldTrain,
                ],
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
            (fields, empties)
        });

        match (ty, collections) {
            (StructType::Named, collections) => {
                if let Some((fields, empties)) = collections {
                    let return_type_generics = if !empties.is_empty() {
                        quote! { <#(#empties),*> }
                    } else {
                        quote! {}
                    };
                    let constructor_fields = fields
                        .into_iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = &empties[i];
                            quote! { #field: #ty }
                        })
                        .collect::<Vec<_>>();

                    quote! {
                        impl #first #ident #second #where_clause {
                            #vis fn #mb_ident() -> #builder_ident #return_type_generics  {
                                #builder_ident {
                                    #(#constructor_fields),*
                                }
                            }
                        }
                    }
                } else {
                    quote! {
                        impl #first #ident #second #where_clause {
                            #vis fn #mb_ident() -> #builder_ident  {
                                #builder_ident {}
                            }
                        }
                    }
                }
            }
            (StructType::Unnamed, data) => {
                if let Some((_, empties)) = data {
                    let return_type_generics = if !empties.is_empty() {
                        quote! { <#(#empties),*> }
                    } else {
                        quote! {}
                    };

                    quote! {
                        impl #first #ident #second #where_clause {
                            #vis fn #mb_ident() -> #builder_ident #return_type_generics  {
                                #builder_ident ( #(#empties),* )
                            }
                        }
                    }
                } else {
                    quote! {
                        impl #first #ident #second #where_clause {
                            #vis fn #mb_ident() -> #builder_ident  {
                                #builder_ident ()
                            }
                        }
                    }
                }
            }
        }
    }
}

mod builder_impl {
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

        let collections = map.get(mapkey::startp::BUILDER_FIELD).map(|start| {
            let mut fields = Vec::new();
            let mut generics = Vec::new();
            let mut empties = Vec::new();
            let mut addeds = Vec::new();
            let action = |graph: &StructGraph, _edge, node| match &graph[node] {
                StructElement::BuilderField(rc) => fields.push(Rc::clone(rc)),
                StructElement::BuilderGeneric(rc) => generics.push(Rc::clone(rc)),
                StructElement::BuilderStateEmpty(rc) => empties.push(Rc::clone(rc)),
                StructElement::BuilderStateAdded(rc) => addeds.push(Rc::clone(rc)),
                _ => {}
            };
            let _: () = traverse(
                graph,
                &[
                    &StructRelation::BuilderStatePair,
                    &StructRelation::BuilderFieldToBuilderState,
                    &StructRelation::BuilderFieldToBuilderGeneric,
                    &StructRelation::BuilderFieldTrain,
                ],
                *start,
                true,
                action,
            );
            assert!(fields.len() == generics.len() && empties.len() == addeds.len());
            (fields, generics, empties, addeds)
        });

        let impl_blocks = collections.map(|(fields, generics, empties, addeds)| {
            let mut impl_blocks = Vec::with_capacity(fields.len());
            for (i0, field) in fields.iter().enumerate() {
                let gfirst = generics
                    .iter()
                    .enumerate()
                    .filter_map(|(i1, f)| if i0 == i1 { None } else { Some(Rc::clone(f)) })
                    .collect::<Vec<_>>();
                let gfirst = if !gfirst.is_empty() {
                    quote! { <#(#gfirst),*> }
                } else {
                    quote! {}
                };

                let gsecond = generics
                    .iter()
                    .enumerate()
                    .map(|(i1, f)| {
                        if i0 == i1 {
                            Rc::clone(&empties[i1])
                        } else {
                            Rc::clone(f)
                        }
                    })
                    .collect::<Vec<_>>();
                let gsecond = if !gsecond.is_empty() {
                    quote! { <#(#gsecond),*> }
                } else {
                    quote! {}
                };

                let gmethod = if !addeds[i0].generics.is_empty() {
                    let generics = &addeds[i0].generics;
                    quote! { <#(#generics),*> }
                } else {
                    quote! {}
                };

                let param = {
                    let added_type = &addeds[i0].ty;
                    quote! { #field: #added_type }
                };

                let generics_result = generics
                    .iter()
                    .enumerate()
                    .map(|(i1, f)| {
                        if i0 == i1 {
                            let ident = &addeds[i1].ident;
                            let generics = if !addeds[i1].generics.is_empty() {
                                let generics = &addeds[i1]
                                    .generics
                                    .iter()
                                    .map(|generic| match generic.as_ref() {
                                        syn::GenericParam::Lifetime(lifetime_param) => {
                                            let lt = &lifetime_param.lifetime;
                                            quote! { #lt }
                                        }
                                        syn::GenericParam::Type(type_param) => {
                                            let ident = &type_param.ident;
                                            quote! { #ident }
                                        }
                                        syn::GenericParam::Const(const_param) => {
                                            let ident = &const_param.ident;
                                            quote! { #ident }
                                        }
                                    })
                                    .collect::<Vec<_>>();
                                quote! { <#(#generics),*> }
                            } else {
                                quote! {}
                            };
                            quote! { #ident #generics }
                        } else {
                            quote! { #f }
                        }
                    })
                    .collect::<Vec<_>>();
                let gresult = if !generics_result.is_empty() {
                    quote! { <#(#generics_result),*> }
                } else {
                    quote! {}
                };

                let whc = if !addeds[i0].where_predicates.is_empty() {
                    let where_predicates = &addeds[i0].where_predicates;
                    quote! { where #(#where_predicates),* }
                } else {
                    quote! {}
                };

                let constructor_data = match ty {
                    StructType::Named => {
                        let fields = fields
                            .iter()
                            .enumerate()
                            .map(|(i1, field)| {
                                if i0 == i1 {
                                    let added_ident = &addeds[i1].ident;
                                    let phantoms = if !addeds[i1].phantoms.is_empty() {
                                        let phantoms = &addeds[i1]
                                            .phantoms
                                            .iter()
                                            .map(|_| quote! { std::marker::PhantomData })
                                            .collect::<Vec<_>>();
                                        quote! { , #(#phantoms),* }
                                    } else {
                                        quote! {}
                                    };
                                    quote! { #field: #added_ident(#field #phantoms) }
                                } else {
                                    quote! { #field: self.#field }
                                }
                            })
                            .collect::<Vec<_>>();
                        quote! {{
                            #(#fields),*
                        }}
                    }
                    StructType::Unnamed => {
                        let fields = fields
                            .iter()
                            .enumerate()
                            .map(|(i1, field)| {
                                if i0 == i1 {
                                    let added_ident = &addeds[i1].ident;
                                    let phantoms = if !addeds[i1].phantoms.is_empty() {
                                        let phantoms = &addeds[i1]
                                            .phantoms
                                            .iter()
                                            .map(|_| quote! { std::marker::PhantomData })
                                            .collect::<Vec<_>>();
                                        quote! { , #(#phantoms),* }
                                    } else {
                                        quote! {}
                                    };
                                    quote! { #added_ident(#field, #phantoms) }
                                } else {
                                    let i = syn::Index::from(i1);
                                    quote! { self.#i }
                                }
                            })
                            .collect::<Vec<_>>();
                        quote! {(
                           #(#fields),*
                        )}
                    }
                };

                impl_blocks.push(quote! {
                    impl #gfirst #builder_ident #gsecond {
                        #vis fn #field #gmethod(self, #param) -> #builder_ident #gresult #whc {
                            #builder_ident #constructor_data
                        }
                    }
                });
            }
            impl_blocks
        });
        if let Some(impl_blocks) = impl_blocks {
            quote! { #(#impl_blocks)* }
        } else {
            quote! {}
        }
    }
}

mod builder_build_impl {
    use std::{collections::HashMap, rc::Rc};

    use indexmap::IndexMap;
    use petgraph::{graph::NodeIndex, Direction};
    use proc_macro2::{Span, TokenStream as TokenStream2};
    use quote::quote;

    use crate::{
        graph::{
            mapkey, msg, traverse, BuilderStateAdded, StructElement, StructGraph, StructRelation,
            StructType,
        },
        helper,
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

        let (gfirst, gsecond, gmethod) = match Generics::new(graph, map) {
            Some(Generics {
                first,
                second,
                method,
            }) => (Some(first), Some(second), Some(method)),
            None => (None, None, None),
        };
        let where_clause =
            WhereClause::new(graph, map).map(|WhereClause(where_clause)| where_clause);
        let (fields, addeds) = match Collections::new(graph, map, ty) {
            Some(Collections { fields, addeds }) => (Some(fields), Some(addeds)),
            None => (None, None),
        };

        let gsecond = gsecond.map(|gsecond| {
            if !gsecond.is_empty() {
                Some(quote! { <#(#gsecond),*> })
            } else {
                None
            }
        });
        let fields = ty.wrap_fields(fields);
        quote! {
            impl #gfirst #builder_ident #addeds #where_clause {
                #vis fn build #gmethod (self) -> #ident #gsecond {
                    #ident #fields
                }
            }
        }
    }

    struct Generics {
        first: TokenStream2,
        /// The second must come without brackets because of the possibility of it being used later.
        second: Vec<TokenStream2>,
        method: TokenStream2,
    }

    impl Generics {
        fn new(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Option<Self> {
            let start = map.get(mapkey::startp::GENERICS)?;

            let action_generics = |graph: &StructGraph, _edge, node_generic| {
                let StructElement::Generic(generic) = &graph[node_generic] else {
                    panic!("{}", msg::node::GENERIC);
                };
                let field_conn = graph
                    .edges_directed(node_generic, Direction::Incoming)
                    .any(|p| {
                        let edge = p.weight();
                        edge == &StructRelation::FieldGenericInMainType
                            || edge == &StructRelation::FieldGenericInMainLifetime
                            || edge == &StructRelation::FieldGenericInMainConst
                    });
                (Rc::clone(&generic.syn), field_conn)
            };

            let generics: Vec<_> = traverse(
                graph,
                &[&StructRelation::GenericTrain],
                *start,
                true,
                action_generics,
            );

            if !generics.is_empty() {
                let mut first = TokenStream2::new();
                let mut second = Vec::with_capacity(generics.len());
                let mut method = TokenStream2::new();

                for (generic, field_connection) in generics {
                    match generic.as_ref() {
                        syn::GenericParam::Lifetime(lifetime_param) => {
                            first.extend(quote! { #generic, });
                            let lt = &lifetime_param.lifetime;
                            second.push(quote! { #lt });
                        }
                        syn::GenericParam::Type(type_param) => {
                            first.extend(quote! { #generic, });
                            let ident = &type_param.ident;
                            second.push(quote! { #ident });
                        }
                        syn::GenericParam::Const(const_param) => {
                            let ident = &const_param.ident;
                            if !field_connection {
                                method.extend(quote! { #generic, });
                            } else {
                                first.extend(quote! { #generic, });
                            }
                            second.push(quote! { #ident });
                        }
                    }
                }

                Some(Self {
                    first: quote! { <#first> },
                    second,
                    method: if !method.is_empty() {
                        quote! { <#method> }
                    } else {
                        TokenStream2::new() // Empty TokenStream2 instead of `quote! {}` to keep consistency
                    },
                })
            } else {
                None
            }
        }
    }

    struct WhereClause(TokenStream2);

    impl WhereClause {
        fn new(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Option<Self> {
            let start = map.get(mapkey::startp::BUILDER_FIELD)?;

            // Traverse to gather the added builder states
            let action = |graph: &StructGraph, _edge, node| {
                if let StructElement::BuilderStateAdded(rc) = &graph[node] {
                    Some(Rc::clone(rc))
                } else {
                    None
                }
            };
            let addeds: Vec<_> = traverse(
                graph,
                &[
                    &StructRelation::BuilderStatePair,
                    &StructRelation::BuilderFieldToBuilderState,
                    &StructRelation::BuilderFieldTrain,
                ],
                *start,
                true,
                action,
            );

            // Flatten and process the added builder states into where predicates
            let wps = addeds
                .into_iter()
                .flatten()
                .flat_map(|added| rename_orphans(Rc::clone(&added)))
                .map(|f| match f {
                    WPOwnedOrShared::Owned(where_predicate) => quote! { #where_predicate },
                    WPOwnedOrShared::Shared(rc) => quote! { #rc },
                })
                .collect::<Vec<_>>(); // Collect the results into a vector

            // If there are any where predicates, generate the where clause
            if !wps.is_empty() {
                Some(Self(quote! { where #(#wps),* }))
            } else {
                None
            }
        }
    }

    struct Collections {
        fields: TokenStream2,
        addeds: TokenStream2,
    }

    impl Collections {
        fn new(
            graph: &StructGraph,
            map: &IndexMap<String, NodeIndex>,
            ty: &StructType,
        ) -> Option<Self> {
            let start = map.get(mapkey::startp::BUILDER_FIELD)?;

            let mut fields = TokenStream2::new();
            let mut fields_i = 0;
            let mut addeds = TokenStream2::new();

            let action =
                |graph: &StructGraph, _edge, node_builder_field| match &graph[node_builder_field] {
                    StructElement::BuilderField(field) => {
                        let new_field = match ty {
                            StructType::Named => quote! { #field: self.#field.0, },
                            StructType::Unnamed => {
                                let fields_i = syn::Index::from(fields_i);
                                quote! { self.#fields_i.0, }
                            }
                        };
                        fields.extend(new_field);
                        fields_i += 1;
                    }
                    StructElement::BuilderStateAdded(added) => {
                        let ident = &added.ident;
                        let generics = if !added.generics.is_empty() {
                            let mut added_generics = TokenStream2::new();
                            for generic in &added.generics {
                                match generic.as_ref() {
                                    syn::GenericParam::Lifetime(lifetime_param) => {
                                        let lt = &lifetime_param.lifetime;
                                        added_generics.extend(quote! { #lt, });
                                    }
                                    syn::GenericParam::Type(type_param) => {
                                        let ident = &type_param.ident;
                                        added_generics.extend(quote! { #ident, });
                                    }
                                    syn::GenericParam::Const(const_param) => {
                                        let ident = &const_param.ident;
                                        added_generics.extend(quote! { #ident, });
                                    }
                                }
                            }
                            Some(quote! { <#added_generics> })
                        } else {
                            None
                        };
                        addeds.extend(quote! { #ident #generics, });
                    }
                    _ => {}
                };

            let _: () = traverse(
                graph,
                &[
                    &StructRelation::BuilderStatePair,
                    &StructRelation::BuilderFieldToBuilderState,
                    &StructRelation::BuilderFieldTrain,
                ],
                *start,
                true,
                action,
            );

            if !addeds.is_empty() {
                addeds = quote! { <#addeds> }
            };

            Some(Self { fields, addeds })
        }
    }

    enum WPOwnedOrShared {
        Owned(syn::WherePredicate),
        Shared(Rc<syn::WherePredicate>),
    }

    fn rename_orphans(added: Rc<BuilderStateAdded>) -> Vec<WPOwnedOrShared> {
        let mut res = Vec::with_capacity(added.where_predicates.len());

        for wp in added.where_predicates.iter() {
            let mut new_bounds_left = HashMap::new();
            let mut new_bounds_right = HashMap::new();

            /* ✅ #TD77972132 Mark orphans. */
            if let syn::WherePredicate::Type(predicate_type) = wp.as_ref() {
                /* ✅ #TD49524702 Bound Lifetimes. */
                if let Some(blts) = &predicate_type.lifetimes {
                    for (pos_blt, blt) in blts.lifetimes.iter().enumerate() {
                        if let Some(pos_orphan) = added.orphans.iter().position(|orphan| {
                            if let syn::GenericParam::Lifetime(blt) = blt {
                                if let syn::GenericParam::Lifetime(orphan) = orphan {
                                    blt.lifetime == orphan.lifetime
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        }) {
                            let mut new_name = String::new();
                            new_name.push('\'');
                            helper::string::rand_lowercase(8, &mut new_name);
                            new_bounds_left.insert(pos_blt, (pos_orphan, new_name));
                        }
                    }
                }

                /* ✅ #TD57071027 Bounds. */
                for (pos_bound, bound) in predicate_type.bounds.iter().enumerate() {
                    if let syn::TypeParamBound::Lifetime(lt0) = bound {
                        if let Some(pos_orphan) = added.orphans.iter().position(|p| {
                            if let syn::GenericParam::Lifetime(ltp) = p {
                                &ltp.lifetime == lt0
                            } else {
                                false
                            }
                        }) {
                            new_bounds_right.insert(pos_bound, pos_orphan);
                        }
                    }
                }
            }

            /* ✅ #TD05287204 Replace orphan names. */
            if !new_bounds_left.is_empty() {
                if let syn::WherePredicate::Type(predicate_type) = wp.as_ref() {
                    let mut new_predicate_type = predicate_type.clone();

                    if let Some(blts) = &mut new_predicate_type.lifetimes {
                        for (pos_blt, (_pos_orphan, new_name)) in new_bounds_left.iter() {
                            let new_gp = syn::GenericParam::Lifetime(syn::LifetimeParam::new(
                                syn::Lifetime::new(new_name, Span::call_site()),
                            ));
                            blts.lifetimes[*pos_blt] = new_gp;
                        }
                    }

                    for (pos_bound, pos_orphan0) in new_bounds_right {
                        if let Some(new_name) = new_bounds_left.iter().find_map(
                            |(_pos_blt, (pos_orphan1, new_name))| {
                                if &pos_orphan0 == pos_orphan1 {
                                    Some(new_name)
                                } else {
                                    None
                                }
                            },
                        ) {
                            let new_tpb = syn::TypeParamBound::Lifetime(syn::Lifetime::new(
                                new_name,
                                Span::call_site(),
                            ));
                            new_predicate_type.bounds[pos_bound] = new_tpb;
                        }
                    }

                    res.push(WPOwnedOrShared::Owned(syn::WherePredicate::Type(
                        new_predicate_type,
                    )));
                }
            } else {
                res.push(WPOwnedOrShared::Shared(Rc::clone(wp)));
            }
        }

        res
    }
}
