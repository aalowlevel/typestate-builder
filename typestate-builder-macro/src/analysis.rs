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
            Self::Attrs(attrs) => {
                write!(f, "Attrs(")?;
                for (i, attr) in attrs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", syn_element_to_string(attr))?;
                }
                write!(f, ")")
            }
            Self::Vis(visibility) => {
                write!(f, "Vis(")?;
                write!(f, "{}", syn_element_to_string(visibility))?;
                write!(f, ")")
            }
            Self::Ident(ident) => {
                write!(f, "Ident(")?;
                write!(f, "{}", syn_element_to_string(ident))?;
                write!(f, ")")
            }
            Self::GenLifetimes(lifetimes) => {
                write!(f, "GenLifetimes(")?;
                for (i, lifetime) in lifetimes.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", syn_element_to_string(lifetime))?;
                }
                write!(f, ")")
            }
            Self::GenConsts(const_params) => {
                write!(f, "GenConsts(")?;
                for (i, const_param) in const_params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", syn_element_to_string(const_param))?;
                }
                write!(f, ")")
            }
            Self::GenTypes(type_params) => {
                write!(f, "GenTypes(")?;
                for (i, type_param) in type_params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", syn_element_to_string(type_param))?;
                }
                write!(f, ")")
            }
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

fn syn_element_to_string<E>(element: &E) -> String
where
    E: ToTokens,
{
    element.to_token_stream().to_string()
}
