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

#![warn(missing_docs)]

//! This crate provides the `TypestateBuilder` derive macro for generating a
//! typestate-pattern builder for structs.
//!
//! # Typestate Pattern
//! The typestate pattern helps ensure that a struct is initialized in steps,
//! enforcing that certain fields must be set before others. This prevents
//! the creation of incomplete structs and provides compile-time safety for
//! field initialization.
//!
//! Example usage:
//!
//! ```rust
//! #[derive(TypestateBuilder)]
//! struct Person {
//!     name: String,
//!     age: u32,
//!     email: Option<String>,
//! }
//!
//! let person = Person::builder()
//!     .name("Alice".to_string())
//!     .age(30)
//!     .email(Some("alice@example.com".to_string()))
//!     .build();
//! ```

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::proc_macro_error;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, GenericParam,
    Generics, Ident, Lifetime, Type, WhereClause, WherePredicate,
};
use titlecase::titlecase;

/// Struct to hold the generated output of the `TypestateBuilder` macro.
struct TypestateBuilderOutPut {
    /// A collection of state structs used to represent the typestate of each field.
    state_structs: Vec<TokenStream2>,
    /// The builder struct itself, which is used to construct the final type.
    builder_struct: TokenStream2,

    /** Beginning point for the builder struct. */
    builder_method: TokenStream2,
    /// A collection of methods for setting the fields of the builder in the correct order.
    builder_constructor_methods: Vec<TokenStream2>,
    /// The final build method that assembles the struct after all required fields are set.
    build_method: TokenStream2,
}

/// The `TypestateBuilder` derive macro generates builder pattern code based on the
/// typestate pattern. It provides compile-time guarantees that all necessary fields
/// are initialized before building the final struct.
///
/// # Panics
/// This macro will panic if applied to a non-struct type (such as an enum or union).
#[proc_macro_derive(TypestateBuilder)]
#[proc_macro_error]
pub fn typestate_builder_derive(input: TokenStream) -> TokenStream {
    // Parse the input token stream into a `DeriveInput` structure.
    let input = parse_macro_input!(input as DeriveInput);

    // Match the type of struct and generate the appropriate builder code.
    let TypestateBuilderOutPut {
        state_structs,
        builder_struct,
        builder_method,
        builder_constructor_methods,
        build_method,
    } = match &input.data {
        // Handle named field structs.
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => generate_named_struct_code(&input, fields),
            // Handle tuple structs (structs with unnamed fields).
            Fields::Unnamed(fields) => generate_tuple_struct_code(&input, fields),
            // Handle unit structs (structs with no fields).
            Fields::Unit => generate_unit_struct_code(&input),
        },
        // Panic if applied to anything other than a struct.
        _ => panic!("TypestateBuilder only supports structs"),
    };

    // Combine the generated code into a final token stream.
    let output = quote! {
        #(#state_structs)*
        #builder_struct
        #builder_method
        #(#builder_constructor_methods)*
        #build_method
    };

    // Return the generated code.
    output.into()
}

/// Generates the builder code for structs with named fields.
///
/// # Arguments
///
/// * `input` - The parsed `DeriveInput` containing metadata about the struct.
/// * `fields` - The named fields of the struct.
///
/// # Returns
///
/// A `TypestateBuilderOutPut` struct containing the generated state structs,
/// builder struct, builder methods, and the build method for the named struct.
fn generate_named_struct_code(input: &DeriveInput, fields: &FieldsNamed) -> TypestateBuilderOutPut {
    let vis = &input.vis;
    let ident = &input.ident;
    let generics = &input.generics;
    let where_clause = &generics.where_clause;
    let where_clause_parsed = where_clause.as_ref().map(|f| parse_where_clause(f));

    // Ident for the builder struct.
    let builder_struct_ident = format_ident!("{}Builder", ident);

    // State structs & collect data.
    struct FieldData<'a> {
        ident: &'a Ident,
        ident_titlecase: Ident,
        state_struct_empty: Ident,
        state_struct_added: Ident,
        ty: &'a Type,
        generics: Vec<GenericParamKind<'a>>,
        where_predicates: Option<Vec<&'a WherePredicate>>,
    }
    let mut field_data = Vec::new();
    let mut state_structs = Vec::new();

    // Extract generics of the main struct.
    let generic_params_main = GenericParamKind::from_generics(generics);

    // Iterate to collect some data.
    for field in fields.named.iter() {
        // Ident
        let field_ident = field
            .ident
            .as_ref()
            .expect("field name of named struct cannot be `None`");
        let field_ident_titlecase = format_ident!("{}", titlecase(&field_ident.to_string()));

        // Type
        let field_type = &field.ty;

        // Extract generics
        let field_generics = check_type_for_generics(&field.ty, &generic_params_main);
        let field_generics_ts =
            create_generics_from_collection(&GenericParamKind::to_token_stream(&field_generics));

        // Associated where predicates.
        let where_predicates = where_clause_parsed.as_ref().map(|f| {
            let mut field_predicates = Vec::new();
            for (wh_ident, predicate) in f {
                for field_generic in field_generics.iter() {
                    match field_generic {
                        GenericParamKind::Type(fgi) => {
                            if fgi == wh_ident {
                                field_predicates.push(*predicate);
                            }
                        }
                        GenericParamKind::Lifetime(_) => unimplemented!(),
                        GenericParamKind::Const(_) => unimplemented!(),
                    }
                }
            }
            field_predicates
        });

        // State structs
        let state_struct_empty =
            format_ident!("{}{}Empty", builder_struct_ident, field_ident_titlecase);
        let state_struct_added =
            format_ident!("{}{}Added", builder_struct_ident, field_ident_titlecase);
        let where_clause = create_where_clause_from_collection(&where_predicates);
        state_structs.push(quote! {
            struct #state_struct_added #field_generics_ts (#field_type) #where_clause;
            struct #state_struct_empty;
        });

        field_data.push(FieldData {
            ident: field_ident,
            ident_titlecase: field_ident_titlecase,
            state_struct_empty,
            state_struct_added,
            ty: field_type,
            generics: field_generics,
            where_predicates,
        });
    }

    // Builder struct.
    let builder_generics: Vec<_> = field_data.iter().map(|f| &f.ident_titlecase).collect();
    let builder_fields: Vec<_> = field_data
        .iter()
        .map(|f| {
            let ident = f.ident;
            let ident_titlecase = &f.ident_titlecase;
            quote! { #ident: #ident_titlecase }
        })
        .collect();
    let builder_struct = quote! {
        #vis struct #builder_struct_ident <#(#builder_generics),*> { #(#builder_fields),* }
    };

    // Builder method.
    let builder_method_generics: Vec<_> =
        field_data.iter().map(|f| &f.state_struct_empty).collect();
    let builder_method_builder_fields: Vec<_> = field_data
        .iter()
        .map(|f| {
            let field_name = f.ident;
            let field_type = &f.state_struct_empty;
            quote! { #field_name: #field_type }
        })
        .collect();
    let builder_method = quote! {
        impl #generics #ident #generics #where_clause {
            #vis fn builder() -> #builder_struct_ident < #(#builder_method_generics),* > {
                #builder_struct_ident {
                    #(#builder_method_builder_fields),*
                }
            }
        }
    };

    // Builder constructor methods.
    let mut builder_constructor_methods = Vec::new();
    for (i0, field0) in field_data.iter().enumerate() {
        let field0_name = field0.ident;
        let field0_type = field0.ty;
        let field_struct_added = &field0.state_struct_added;

        let mut declare_generics = Vec::new();
        let mut builder_generics = Vec::new();

        let mut builder_generics_res = Vec::new();
        let mut builder_data = Vec::new();

        // Loop the same structure again to get extra info.
        for (i1, field1) in field_data.iter().enumerate() {
            let field1_name = field1.ident;
            let field1_titlecase = &field1.ident_titlecase;

            // It's at the field.
            if i0 == i1 {
                builder_generics.push(&field1.state_struct_empty);
                builder_generics_res.push((&field1.state_struct_added, true));
                builder_data.push(quote! { #field1_name: #field_struct_added  (#field1_name) });
            } else {
                declare_generics.push(quote! { #field1_titlecase });
                builder_generics.push(field1_titlecase);
                builder_generics_res.push((field1_titlecase, false));
                builder_data.push(quote! { #field1_name: self.#field1_name });
            }
        }

        // Produce token stream of generics.
        let declare_generics = create_generics_from_collection(&declare_generics);
        let builder_generics = create_generics_from_collection(&builder_generics);
        let method_generics =
            create_generics_from_collection(&GenericParamKind::to_token_stream(&field0.generics));

        // Where clause for method.
        let method_where_clause = create_where_clause_from_collection(&field0.where_predicates);

        // Create generics part of the return type of the method.
        let builder_generics_res = builder_generics_res
            .into_iter()
            .map(|(ty, is_added)| {
                if is_added {
                    let ty_generics = create_generics_from_collection(
                        &GenericParamKind::to_token_stream(&field0.generics),
                    );
                    quote! { #ty #ty_generics }
                } else {
                    quote! { #ty }
                }
            })
            .collect::<Vec<_>>();
        let builder_generics_res = create_generics_from_collection(&builder_generics_res);

        // Shape impl block.
        builder_constructor_methods.push(quote! {
            impl #declare_generics #builder_struct_ident #builder_generics {
                #vis fn #field0_name #method_generics (self, #field0_name: #field0_type) -> #builder_struct_ident #builder_generics_res #method_where_clause {
                    #builder_struct_ident {
                        #(#builder_data),*
                    }
                }
            }
        });
    }

    // Build method.
    let build_impl_block_generics = field_data
        .iter()
        .map(|f| &f.state_struct_added)
        .collect::<Vec<_>>();
    let build_impl_block_generics = create_generics_from_collection(&build_impl_block_generics);
    let build_method_data = field_data
        .iter()
        .map(|f| {
            let field_name = f.ident;
            quote! { #field_name: self.#field_name.0 }
        })
        .collect::<Vec<_>>();
    let build_method = quote! {
        impl #builder_struct_ident #build_impl_block_generics {
            #vis fn build(self) -> #ident {
                #ident {
                    #(#build_method_data),*
                }
            }
        }
    };

    TypestateBuilderOutPut {
        state_structs,
        builder_struct,
        builder_method,
        builder_constructor_methods,
        build_method,
    }
}

/// Generates the builder code for tuple structs (i.e., structs with unnamed fields).
///
/// # Arguments
///
/// * `input` - The parsed `DeriveInput` containing metadata about the struct.
/// * `fields` - The unnamed fields of the struct.
///
/// # Returns
///
/// A `TypestateBuilderOutPut` struct containing the generated state structs,
/// builder struct, builder methods, and the build method for the tuple struct.
fn generate_tuple_struct_code(
    input: &DeriveInput,
    fields: &FieldsUnnamed,
) -> TypestateBuilderOutPut {
    todo!()
}

/// Generates the builder code for unit structs (i.e., structs without fields).
///
/// # Arguments
///
/// * `input` - The parsed `DeriveInput` containing metadata about the struct.
///
/// # Returns
///
/// A `TypestateBuilderOutPut` struct containing the generated state structs,
/// builder struct, builder methods, and the build method for the unit struct.
fn generate_unit_struct_code(input: &DeriveInput) -> TypestateBuilderOutPut {
    todo!()
}

#[derive(Clone)]
enum GenericParamKind<'a> {
    Type(&'a Ident),
    Lifetime(&'a Lifetime),
    Const(&'a Ident),
}

impl<'a> GenericParamKind<'a> {
    fn to_token_stream(generics: &[GenericParamKind<'a>]) -> Vec<TokenStream2> {
        generics
            .iter()
            .map(|f| match f {
                GenericParamKind::Type(f) => quote! { #f },
                GenericParamKind::Lifetime(f) => quote! { #f },
                GenericParamKind::Const(f) => quote! { #f },
            })
            .collect()
    }
    fn from_generics(generics: &'a Generics) -> Vec<GenericParamKind<'a>> {
        generics
            .params
            .iter()
            .map(|param| match param {
                GenericParam::Type(type_param) => GenericParamKind::Type(&type_param.ident),
                GenericParam::Lifetime(lifetime_def) => {
                    GenericParamKind::Lifetime(&lifetime_def.lifetime)
                }
                GenericParam::Const(const_param) => GenericParamKind::Const(&const_param.ident),
            })
            .collect()
    }
}

fn check_type_for_generics<'a>(
    ty: &Type,
    generic_params_main: &[GenericParamKind<'a>],
) -> Vec<GenericParamKind<'a>> {
    let mut generics_params_field = Vec::new();

    match ty {
        // Handles cases like T
        Type::Path(type_path) => {
            for segment in &type_path.path.segments {
                // Check if the type or const is present in the generic params
                if let Some(param) = generic_params_main.iter().find(|p| match p {
                    GenericParamKind::Type(ident) | GenericParamKind::Const(ident) => {
                        *ident == &segment.ident
                    }
                    _ => false,
                }) {
                    generics_params_field.push(param.clone());
                }

                // Handle nested generics like Option<T>
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        match arg {
                            syn::GenericArgument::Type(nested_type) => {
                                // Recursively check nested types
                                generics_params_field.extend(check_type_for_generics(
                                    nested_type,
                                    generic_params_main,
                                ));
                            }
                            syn::GenericArgument::Lifetime(lifetime) => {
                                // Check lifetimes inside nested generics
                                if let Some(param) = generic_params_main.iter().find(|p| match p {
                                    GenericParamKind::Lifetime(lt) => *lt == lifetime,
                                    _ => false,
                                }) {
                                    generics_params_field.push(param.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        // Handles cases like &'a T
        Type::Reference(type_ref) => {
            // Check for the lifetime in the reference
            if let Some(lifetime) = &type_ref.lifetime {
                if let Some(param) = generic_params_main.iter().find(|p| match p {
                    GenericParamKind::Lifetime(lt) => *lt == lifetime,
                    _ => false,
                }) {
                    generics_params_field.push(param.clone());
                }
            }

            // Recursively check the type inside the reference (&'a T -> T)
            generics_params_field
                .extend(check_type_for_generics(&type_ref.elem, generic_params_main));
        }

        _ => {}
    }

    generics_params_field
}

fn parse_where_clause(where_clause: &WhereClause) -> Vec<(&Ident, &WherePredicate)> {
    let mut res = Vec::new();

    for predicate in where_clause.predicates.iter() {
        if let WherePredicate::Type(ty) = &predicate {
            if let Type::Path(type_path) = &ty.bounded_ty {
                if let Some(ident) = type_path.path.get_ident() {
                    res.push((ident, predicate));
                }
            }
        }
    }

    res
}

fn create_generics_from_collection<T>(collection: &Vec<T>) -> TokenStream2
where
    T: ToTokens,
{
    if !collection.is_empty() {
        quote! { < #(#collection),* > }
    } else {
        quote! {}
    }
}

fn create_where_clause_from_collection<T>(where_predicates: &Option<Vec<T>>) -> TokenStream2
where
    T: ToTokens,
{
    if let Some(where_predicates) = where_predicates {
        quote! { where #(#where_predicates),* }
    } else {
        quote! {}
    }
}
