use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};

use crate::{
    graph::{traverse, StructElement, StructGraph, StructType, IDENT, TYPE, VIS},
    helper::{NODE_IDENT_MSG, NODE_TYPE_MSG, NODE_VIS_MSG},
};

pub(super) fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Option<TokenStream2> {
    let StructElement::Visibility(vis) = &graph[*map.get(VIS)?] else {
        panic!("{}", NODE_VIS_MSG);
    };
    let StructElement::Ident(ident) = &graph[*map.get(IDENT)?] else {
        panic!("{}", NODE_IDENT_MSG);
    };
    let ident = format_ident!("{}Builder", ident);
    let StructElement::Type(ty) = &graph[*map.get(TYPE)?] else {
        panic!("{}", NODE_TYPE_MSG);
    };

    let data = match ty {
        StructType::Named => quote! { {} },
        StructType::Unnamed => quote! { (); },
        StructType::Unit => quote! { ; },
    };

    Some(quote! {
        #vis struct #ident #data
    })
}
