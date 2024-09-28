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

use std::collections::HashSet;

use petgraph::Graph;
use serde::{ser::SerializeStruct, Serialize};
use syn::{
    Attribute, Expr, ExprPath, GenericArgument, GenericParam, Ident, Lifetime, PathArguments, Type,
    TypeArray, Visibility, WherePredicate,
};
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
                &field.syn.to_adapter(),
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
    FieldGenericsInMain,
    FieldGenericsInWhereClause,
}

pub type StructGraph = Graph<StructElement, StructRelation>;

#[derive(Debug)]
pub struct Field {
    pub syn: syn::Field,
    pub idents: HashSet<Ident>,
    pub lifetimes: HashSet<Lifetime>,
    pub const_params: HashSet<Ident>,
}

impl Serialize for Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut res = serializer.serialize_struct("Field", 1)?;
        res.serialize_field("syn", &self.syn.to_adapter())?;
        res.end()
    }
}

impl Field {
    pub fn list(&mut self) {
        Self::list_type(
            &self.syn.ty,
            &mut self.idents,
            &mut self.lifetimes,
            &mut self.const_params,
        );
    }

    fn list_type(
        ty: &Type,
        idents: &mut HashSet<Ident>,
        lifetimes: &mut HashSet<Lifetime>,
        const_params: &mut HashSet<Ident>,
    ) {
        match ty {
            Type::Array(TypeArray { elem, len, .. }) => {
                Self::list_type(elem, idents, lifetimes, const_params);
                Self::handle_const_expr(len, const_params);
            }
            Type::BareFn(type_bare_fn) => {
                for input in &type_bare_fn.inputs {
                    Self::list_type(&input.ty, idents, lifetimes, const_params);
                }
                if let syn::ReturnType::Type(_, return_type) = &type_bare_fn.output {
                    Self::list_type(return_type, idents, lifetimes, const_params);
                }
            }
            Type::Group(type_group) => {
                Self::list_type(&type_group.elem, idents, lifetimes, const_params)
            }
            Type::ImplTrait(type_impl_trait) => {
                for bound in &type_impl_trait.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            Type::Macro(type_macro) => {
                if let Some(ident) = type_macro.mac.path.get_ident() {
                    idents.insert(ident.clone());
                }
            }
            Type::Paren(type_paren) => {
                Self::list_type(&type_paren.elem, idents, lifetimes, const_params)
            }
            Type::Path(type_path) => {
                Self::handle_path(&type_path.path, idents, lifetimes, const_params);
                if let Some(qself) = &type_path.qself {
                    Self::list_type(&qself.ty, idents, lifetimes, const_params);
                }
            }
            Type::Ptr(type_ptr) => Self::list_type(&type_ptr.elem, idents, lifetimes, const_params),
            Type::Reference(type_reference) => {
                if let Some(lt) = &type_reference.lifetime {
                    lifetimes.insert(lt.clone());
                }
                Self::list_type(&type_reference.elem, idents, lifetimes, const_params)
            }
            Type::Slice(type_slice) => {
                Self::list_type(&type_slice.elem, idents, lifetimes, const_params)
            }
            Type::TraitObject(type_trait_object) => {
                for bound in &type_trait_object.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            Type::Tuple(type_tuple) => {
                for elem in &type_tuple.elems {
                    Self::list_type(elem, idents, lifetimes, const_params);
                }
            }
            _ => {}
        }
    }

    fn handle_path(
        path: &syn::Path,
        idents: &mut HashSet<Ident>,
        lifetimes: &mut HashSet<Lifetime>,
        const_params: &mut HashSet<Ident>,
    ) {
        for segment in &path.segments {
            idents.insert(segment.ident.clone());
            match &segment.arguments {
                PathArguments::AngleBracketed(args) => {
                    for arg in &args.args {
                        Self::handle_generic_argument(arg, idents, lifetimes, const_params);
                    }
                }
                PathArguments::Parenthesized(args) => {
                    for ty in &args.inputs {
                        Self::list_type(ty, idents, lifetimes, const_params);
                    }
                    if let syn::ReturnType::Type(_, ty) = &args.output {
                        Self::list_type(ty, idents, lifetimes, const_params);
                    }
                }
                PathArguments::None => {}
            }
        }
    }

    fn handle_generic_argument(
        arg: &GenericArgument,
        idents: &mut HashSet<Ident>,
        lifetimes: &mut HashSet<Lifetime>,
        const_params: &mut HashSet<Ident>,
    ) {
        match arg {
            GenericArgument::Type(ty) => Self::list_type(ty, idents, lifetimes, const_params),
            GenericArgument::Lifetime(lt) => {
                lifetimes.insert(lt.clone());
            }
            GenericArgument::Const(expr) => Self::handle_const_expr(expr, const_params),
            GenericArgument::Constraint(constraint) => {
                idents.insert(constraint.ident.clone());
                for bound in &constraint.bounds {
                    Self::handle_type_param_bound(bound, idents, lifetimes, const_params);
                }
            }
            _ => {}
        }
    }

    fn handle_const_expr(expr: &Expr, const_params: &mut HashSet<Ident>) {
        if let Expr::Path(ExprPath { path, .. }) = expr {
            if path.segments.len() == 1 {
                // Single segment path is likely a const generic parameter
                const_params.insert(path.segments[0].ident.clone());
            }
            // For multi-segment paths, we don't add to const_params
            // as they're likely not const generic parameters
        }
    }

    fn handle_type_param_bound(
        bound: &syn::TypeParamBound,
        idents: &mut HashSet<Ident>,
        lifetimes: &mut HashSet<Lifetime>,
        const_params: &mut HashSet<Ident>,
    ) {
        match bound {
            syn::TypeParamBound::Trait(trait_bound) => {
                Self::handle_path(&trait_bound.path, idents, lifetimes, const_params);
            }
            syn::TypeParamBound::Lifetime(lt) => {
                lifetimes.insert(lt.clone());
            }
            _ => {}
        }
    }
}
