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

use quote::ToTokens;
use syn::{
    Attribute, ConstParam, Field, GenericParam, Ident, LifetimeParam, TypeParam, Visibility,
    WherePredicate,
};

use crate::syn_element_to_string;

pub enum StructElement {
    Visibility(Visibility),
    Ident(Ident),
    Attribute(Attribute),
    Generic(GenericParam),
    WherePredicate(WherePredicate),
    Field(Field),
}

impl std::fmt::Debug for StructElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::Visibility(el) => {
                write!(f, "Visibility(")?;
                syn_element_to_string(el)
            }
            Self::Ident(el) => {
                write!(f, "Ident(")?;
                syn_element_to_string(el)
            }
            Self::Attribute(el) => {
                write!(f, "Attribute(")?;
                syn_element_to_string(el)
            }
            Self::Generic(el) => {
                write!(f, "Generic(")?;
                syn_element_to_string(el)
            }
            Self::WherePredicate(el) => {
                write!(f, "WherePredicate(")?;
                syn_element_to_string(el)
            }
            Self::Field(el) => {
                write!(f, "Field(")?;
                syn_element_to_string(el)
            }
        };
        write!(f, "{})", string)
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
