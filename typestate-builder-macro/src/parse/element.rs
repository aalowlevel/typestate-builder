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
    Attribute, ConstParam, GenericParam, Ident, LifetimeParam, TypeParam, Visibility,
    WherePredicate,
};

use crate::parse::syn_element_to_string;

pub enum StructElement {
    Visibility(Visibility),
    Ident(Ident),
    Attribute(Attribute),
    Generic(GenericParam),
    WherePredicate(WherePredicate),
}

impl std::fmt::Debug for StructElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Visibility(visibility) => {
                write!(f, "Visibility(")?;
                write!(f, "{}", syn_element_to_string(visibility))?;
                write!(f, ")")
            }
            Self::Ident(ident) => {
                write!(f, "Ident(")?;
                write!(f, "{}", syn_element_to_string(ident))?;
                write!(f, ")")
            }
            Self::Attribute(attr) => {
                write!(f, "Attribute(")?;
                write!(f, "{}", syn_element_to_string(attr))?;
                write!(f, ")")
            }
            Self::Generic(attr) => {
                write!(f, "Generic(")?;
                write!(f, "{}", syn_element_to_string(attr))?;
                write!(f, ")")
            }
            Self::WherePredicate(attr) => {
                write!(f, "WherePredicate(")?;
                write!(f, "{}", syn_element_to_string(attr))?;
                write!(f, ")")
            }
        }
    }
}
