use quote::ToTokens;
use syn::{
    Attribute, ConstParam, GenericParam, Ident, LifetimeParam, TypeParam, Visibility,
    WherePredicate,
};

use crate::analysis::syn_element_to_string;

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
