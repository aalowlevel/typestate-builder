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

use serde::Serialize;
use syn::{Attribute, Field, GenericParam, Ident, Visibility, WherePredicate};
use syn_serde::Syn;

pub enum StructElement {
    Visibility(Visibility),
    Ident(Ident),
    Attribute(Attribute),
    Generic(GenericParam),
    WherePredicate(WherePredicate),
    Field(Field),
}

impl Serialize for StructElement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            StructElement::Visibility(visibility) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "Visibility",
                &visibility.to_adapter(),
            ),
            StructElement::Ident(ident) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "Ident",
                &ident.to_adapter(),
            ),
            StructElement::Attribute(attribute) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "Attribute",
                &attribute.to_adapter(),
            ),
            StructElement::Generic(generic_param) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "GenericParam",
                &generic_param.to_adapter(),
            ),
            StructElement::WherePredicate(where_predicate) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "WherePredicate",
                &where_predicate.to_adapter(),
            ),
            StructElement::Field(field) => serializer.serialize_newtype_variant(
                "StructElement",
                0,
                "Field",
                &field.to_adapter(),
            ),
        }
    }
}

impl std::fmt::Debug for StructElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            serde_json::to_string(self).expect("serialize to string pretty")
        )
    }
}

#[derive(Debug)]
pub enum StructRelation {
    AttributeTrain,
    GenericTrain,
    WherePredicateTrain,
    FieldTrain,
    FieldGenerics,
}
