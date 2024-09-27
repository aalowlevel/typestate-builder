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

macro_rules! add_from_syn_list {
    ($graph:expr, $list:expr, $first_node:ident, $node:ident, $edge:ident) => {{
        let mut predecessor = None;
        for (i, attr) in $list.into_iter().enumerate() {
            let successor = if i == 0 {
                $graph.add_node(StructElement::$first_node(attr))
            } else {
                $graph.add_node(StructElement::$node(attr))
            };
            if let Some(predecessor) = predecessor.take() {
                $graph.add_edge(predecessor, successor, StructRelation::$edge);
            }
            predecessor.get_or_insert(successor);
        }
    }};
}

pub fn init(input: DeriveInput) {
    let Data::Struct(data_struct) = input.data else {
        panic!("TypestateBuilder only supports structs");
    };

    let mut graph = Graph::<StructElement, StructRelation>::new();

    // Beginning
    {
        graph.add_node(StructElement::Visibility(input.vis));
        graph.add_node(StructElement::Ident(input.ident));
    }

    add_from_syn_list!(
        graph,
        input.attrs,
        AttributeFirst,
        Attribute,
        AttributeTrain
    );
    add_from_syn_list!(
        graph,
        input.generics.params,
        GenericFirst,
        Generic,
        GenericTrain
    );
    if let Some(where_clause) = input.generics.where_clause {
        add_from_syn_list!(
            graph,
            where_clause.predicates,
            WherePredicateFirst,
            WherePredicate,
            WherePredicateTrain
        );
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
