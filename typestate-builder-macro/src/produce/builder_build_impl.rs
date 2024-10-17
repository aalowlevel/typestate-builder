use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

use crate::graph::StructGraph;

pub(super) fn run(graph: &StructGraph, map: &IndexMap<String, NodeIndex>) -> Option<TokenStream2> {
    Some(quote! {})
}
