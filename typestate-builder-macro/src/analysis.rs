pub mod element;

use std::{collections::HashSet, fs::File, io::Write};

use element::StructElement;
use petgraph::{
    dot::{Config, Dot},
    Graph,
};
use proc_macro_error::emit_call_site_warning;
use quote::ToTokens;
use syn::{
    Attribute, ConstParam, Data, DataStruct, DeriveInput, Fields, FieldsNamed, Ident,
    LifetimeParam, TypeParam, Visibility,
};

#[derive(Debug)]
enum StructRelation {
    GenLeftToRight,
}

pub fn init(input: DeriveInput) {
    let Data::Struct(data_struct) = input.data else {
        panic!("TypestateBuilder only supports structs");
    };

    let mut graph = Graph::<StructElement, StructRelation>::new();
    let mut node_indices = HashSet::new();

    // Basic
    node_indices.insert(graph.add_node(StructElement::Attrs(input.attrs)));
    node_indices.insert(graph.add_node(StructElement::Vis(input.vis)));
    node_indices.insert(graph.add_node(StructElement::Ident(input.ident)));

    // Generics
    node_indices.insert(graph.add_node(StructElement::GenLifetimes(
        input.generics.lifetimes().collect(),
    )));
    node_indices.insert(graph.add_node(StructElement::GenConsts(
        input.generics.const_params().collect(),
    )));
    node_indices.insert(graph.add_node(StructElement::GenTypes(
        input.generics.type_params().collect(),
    )));

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
