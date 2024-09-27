pub mod element;
pub mod relation;

use std::{collections::HashSet, fs::File, io::Write};

use element::StructElement;
use petgraph::{
    dot::{Config, Dot},
    Graph,
};
use proc_macro_error::emit_call_site_warning;
use quote::ToTokens;
use relation::StructRelation;
use syn::{
    Attribute, ConstParam, Data, DataStruct, DeriveInput, Fields, FieldsNamed, Ident,
    LifetimeParam, TypeParam, Visibility,
};

pub fn init(input: DeriveInput) {
    let Data::Struct(data_struct) = input.data else {
        panic!("TypestateBuilder only supports structs");
    };

    let mut graph = Graph::<StructElement, StructRelation>::new();

    // Beginning
    graph.add_node(StructElement::Attrs(input.attrs));
    graph.add_node(StructElement::Vis(input.vis));
    graph.add_node(StructElement::Ident(input.ident));

    // Generics
    let n_lifetimes = graph.add_node(StructElement::GenLifetimes(
        input.generics.lifetimes().collect(),
    ));
    let n_consts = graph.add_node(StructElement::GenConsts(
        input.generics.const_params().collect(),
    ));
    graph.add_edge(n_lifetimes, n_consts, StructRelation::GenLeftToRight);
    let n_types = graph.add_node(StructElement::GenTypes(
        input.generics.type_params().collect(),
    ));
    graph.add_edge(n_consts, n_types, StructRelation::GenLeftToRight);

    // Generics -> where
    if let Some(where_clause) = &input.generics.where_clause {
        graph.add_node(StructElement::GenWherePreds(
            where_clause.predicates.iter().collect::<Vec<_>>(),
        ));
    }

    write_graph_to_file(&graph, "example.dot");
}

fn write_graph_to_file(
    graph: &Graph<StructElement, StructRelation>,
    filename: &str,
) -> std::io::Result<()> {
    let dot = format!("{:?}", Dot::new(graph));
    let mut file = File::create(filename)?;
    file.write_all(dot.as_bytes())?;
    Ok(())
}

fn syn_element_to_string<E>(element: &E) -> String
where
    E: ToTokens,
{
    element.to_token_stream().to_string()
}
