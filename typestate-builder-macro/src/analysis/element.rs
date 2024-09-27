use quote::ToTokens;
use syn::{Attribute, ConstParam, Ident, LifetimeParam, TypeParam, Visibility, WherePredicate};

use crate::analysis::syn_element_to_string;

pub enum StructElement<'a> {
    Attrs(Vec<Attribute>),
    Vis(Visibility),
    Ident(Ident),
    GenLifetimes(Vec<&'a LifetimeParam>),
    GenConsts(Vec<&'a ConstParam>),
    GenTypes(Vec<&'a TypeParam>),
    GenWherePreds(Vec<&'a WherePredicate>),
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
            Self::GenWherePreds(where_predicates) => {
                write!(f, "GenWherePreds(")?;
                for (i, predicate) in where_predicates.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", syn_element_to_string(predicate))?;
                }
                write!(f, ")")
            }
        }
    }
}
