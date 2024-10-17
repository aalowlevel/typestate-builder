use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};

use crate::{
    graph::{
        traverse, StructElement, StructGraph, StructRelation, StructType, FIELD_START_P, IDENT,
        TYPE, VIS,
    },
    helper::{to_titlecase, NODE_FIELD_MSG, NODE_IDENT_MSG, NODE_TYPE_MSG, NODE_VIS_MSG},
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

    /* ✅ #TD92175615 Traverse on field train. */
    let action = |graph: &StructGraph, _edge, field_node| {
        let StructElement::Field(field) = &graph[field_node] else {
            panic!("{}", NODE_FIELD_MSG);
        };
        let ident_str = field
            .syn
            .ident
            .as_ref()
            .map(|f| f.to_string())
            .unwrap_or_else(|| format!("field{}", field.nth));
        let ident = syn::Ident::new(&ident_str, Span::call_site());
        let ident_tc_str = to_titlecase(&ident_str);
        let ident_tc = syn::Ident::new(&ident_tc_str, Span::call_site());
        (ident, ident_tc)
    };
    let fields = traverse(
        graph,
        Some(&[&StructRelation::FieldTrain]),
        *map.get(FIELD_START_P)?,
        true,
        action,
    );

    /* ✅ #TD78566672 Create generics. */
    let generics = if !fields.is_empty() {
        let generics = fields
            .iter()
            .map(|(_, ident_tc)| quote! { #ident_tc })
            .collect::<Vec<_>>();
        quote! { < #(#generics),* > }
    } else {
        quote! {}
    };

    Some(match ty {
        StructType::Named => {
            let fields_ts = fields
                .iter()
                .map(|(ident, ident_tc)| quote! { #ident: #ident_tc })
                .collect::<Vec<_>>();
            quote! {
                #vis struct #ident #generics {
                    #(#fields_ts),*
                }
            }
        }
        StructType::Unnamed => {
            let fields_ts = fields
                .iter()
                .map(|(_, ident_tc)| quote! { #ident_tc })
                .collect::<Vec<_>>();
            quote! {
                #vis struct #ident #generics ( #(#fields_ts),* );
            }
        }
        StructType::Unit => quote! { ; },
    })
}
