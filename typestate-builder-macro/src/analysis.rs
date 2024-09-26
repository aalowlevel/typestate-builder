use std::{fs::File, io::Write};

use petgraph::{
    dot::{Config, Dot},
    Graph,
};
use quote::ToTokens;
use syn::{
    Attribute, ConstParam, Data, DataStruct, DeriveInput, Fields, FieldsNamed, Ident,
    LifetimeParam, TypeParam, Visibility,
};

enum StructElement<'a> {
    Attrs(Vec<Attribute>),
    Vis(Visibility),
    Ident(Ident),
    GenLifetimes(Vec<&'a LifetimeParam>),
    GenConsts(Vec<&'a ConstParam>),
    GenTypes(Vec<&'a TypeParam>),
}

impl<'a> std::fmt::Debug for StructElement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Attrs(attrs) => f.debug_tuple("Attrs").field(attrs).finish(),
            Self::Vis(arg0) => f.debug_tuple("Vis").field(arg0).finish(),
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::GenLifetimes(arg0) => f.debug_tuple("GenLifetimes").field(arg0).finish(),
            Self::GenConsts(arg0) => f.debug_tuple("GenConsts").field(arg0).finish(),
            Self::GenTypes(arg0) => f.debug_tuple("GenTypes").field(arg0).finish(),
        }
    }
}

#[derive(Debug)]
enum StructRelation {}

pub fn init(input: DeriveInput) {
    let Data::Struct(data_struct) = input.data else {
        panic!("TypestateBuilder only supports structs");
    };

    let mut graph = Graph::<StructElement, StructRelation>::new();

    // Basic
    graph.add_node(StructElement::Attrs(input.attrs));
    graph.add_node(StructElement::Vis(input.vis));
    graph.add_node(StructElement::Ident(input.ident));

    // Generics
    graph.add_node(StructElement::GenLifetimes(
        input.generics.lifetimes().collect(),
    ));
    graph.add_node(StructElement::GenConsts(
        input.generics.const_params().collect(),
    ));
    graph.add_node(StructElement::GenTypes(
        input.generics.type_params().collect(),
    ));

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
