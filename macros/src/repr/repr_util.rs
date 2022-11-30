//! Various macro utilities.

use super::err;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parse::Parser, spanned::Spanned, visit_mut::VisitMut, DataEnum, DeriveInput, Fields};

pub struct Quote<'a>(&'a str);
impl<'a> ToTokens for Quote<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts: TokenStream = syn::parse_str(self.0).unwrap();
        ts.to_tokens(tokens);
    }
}
// Crate name constant for less repetition.
pub const CRATE: Quote = Quote("::repr::repr");

pub fn def_has_non_lifetime_generics(def: &DeriveInput) -> bool {
    def.generics
        .params
        .iter()
        .any(|v| !matches!(v, syn::GenericParam::Lifetime(_)))
}

// Enum utilities.
pub fn enum_is_empty(e: &DataEnum) -> bool {
    e.variants.iter().next().is_none()
}

pub fn enum_is_fieldless(e: &DataEnum) -> bool {
    e.variants
        .iter()
        .all(|v| matches!(v.fields, syn::Fields::Unit))
}

pub fn enum_should_have_all_discriminants(e: &DataEnum) -> syn::Result<()> {
    for v in e.variants.iter() {
        if v.discriminant.is_none() {
            return Err(syn::Error::new(
                v.span(),
                err::FIELDLESS_ENUM_NEEDS_ALL_VALUES,
            ));
        }
    }
    Ok(())
}

pub fn enum_should_have_no_discriminants(e: &DataEnum) -> syn::Result<()> {
    for v in e.variants.iter() {
        if v.discriminant.is_some() {
            return Err(syn::Error::new(v.span(), err::FIELD_ENUM_CANNOT_SET_VALUES));
        }
    }
    Ok(())
}

// Unpack structs and tuples into variables <_0, _1, ...>.
// Optionally assign a custom name to first variable, then start counting from 0.
pub fn unpack_fields(fields: &Fields, first_name: Option<syn::Ident>) -> TokenStream {
    let count = if first_name.is_some() {
        fields.len() - 1
    } else {
        fields.len()
    };

    let idents = first_name
        .into_iter()
        .chain((0..count).map(|i| format_ident!("_{}", i)));

    match fields {
        Fields::Named(fs) => {
            let fields = fs.named.iter().map(|i| i.ident.as_ref().unwrap());
            let ident_map = quote! {
                #(#fields: ref #idents,)*
            };
            quote! { {#ident_map} }
        }
        Fields::Unnamed(_) => {
            let ident_list = quote! { #(ref #idents),* };
            quote! { (#ident_list) }
        }
        _ => quote! {},
    }
}

// Prepend a field to a list of fields.
pub fn prepend_field(fields: &mut Fields, id: &syn::Ident, ty: &syn::Path) {
    // Everything parsed here should be valid.
    match fields {
        Fields::Named(fs) => {
            fs.named.insert(
                0,
                syn::Field::parse_named.parse2(quote! { #id: #ty }).unwrap(),
            );
        }
        Fields::Unnamed(fs) => {
            fs.unnamed
                .insert(0, syn::Field::parse_unnamed.parse2(quote! { #ty }).unwrap());
        }
        Fields::Unit => {
            let f: syn::FieldsUnnamed = syn::parse_quote! { (#ty) };
            *fields = f.into();
        }
    }
}

// Turn a fields struct into struct / enum body, with a semicolon if needed.
pub fn fields_to_body(fields: Fields) -> TokenStream {
    let delim = match fields {
        Fields::Named(_) => quote! {},
        _ => quote! {;},
    };
    quote! { #fields #delim }
}

// Turn usize into int literal without suffix.
pub fn int_literal(lit: usize, span: proc_macro2::Span) -> syn::Lit {
    syn::LitInt::new(&lit.to_string(), span).into()
}

// Convert types of all fields to whatever's returned by f.
pub fn convert_field_types(fields: &mut Fields, f: &mut dyn FnMut(TokenStream) -> TokenStream) {
    let mut make_repr = |fd: &mut syn::Field| {
        let ty = &fd.ty;
        fd.ty = syn::Type::Verbatim(f(quote! { #ty }));
    };

    match fields {
        Fields::Named(f) => {
            for field in f.named.iter_mut() {
                make_repr(field);
            }
        }
        Fields::Unnamed(f) => {
            for field in f.unnamed.iter_mut() {
                make_repr(field);
            }
        }
        _ => (),
    }
}

pub fn convert_field_types_to_repr(fields: &mut Fields) {
    let mut conv = |ty| quote! { <#ty as #CRATE::HasRepr> };
    convert_field_types(fields, &mut conv)
}

// Also converts lifetimes to static, since Raw types should stay the same.
pub fn convert_field_types_to_raw(fields: &mut Fields) {
    let mut conv = |ty| quote! { <#ty as #CRATE::HasRepr>::Raw };
    statify_lifetimes(fields);
    convert_field_types(fields, &mut conv)
}

// Call raw_is_valid for unpacked fields <_0, _1, ...>.
pub fn call_fields_raw_is_valid(fields: &Fields) -> TokenStream {
    let mut fd = fields.clone();
    statify_lifetimes(&mut fd);
    convert_field_types_to_repr(&mut fd);

    let idents = (0..fields.len()).map(|i| format_ident!("_{}", i));
    let types = fd.iter().map(|f| &f.ty);
    quote! {
        #(#types::raw_is_valid(#idents)?;)*
    }
}

pub struct StatifyLifetimes;
impl syn::visit_mut::VisitMut for StatifyLifetimes {
    fn visit_lifetime_mut(&mut self, l: &mut syn::Lifetime) {
        l.ident = syn::Ident::new("static", Span::call_site());
        syn::visit_mut::visit_lifetime_mut(self, l)
    }
}

pub fn statify_lifetimes(f: &mut Fields) {
    StatifyLifetimes.visit_fields_mut(f)
}

pub fn ident_with_generics(def: &DeriveInput) -> TokenStream {
    let generics = if def.generics.params.is_empty() {
        quote! {}
    } else {
        let generics = &def.generics.params;
        quote! { <#generics> }
    };
    let ident = &def.ident;
    quote! { #ident #generics }
}

// Generate an impl statement, taking into account generics.
pub fn impl_statement(def: &DeriveInput, impl_: TokenStream, for_: TokenStream) -> TokenStream {
    if def.generics.params.is_empty() {
        quote! { impl #impl_ for #for_ }
    } else {
        let generics = &def.generics.params;
        let where_clause = &def.generics.where_clause;
        quote! { impl<#generics> #impl_ for #for_ #where_clause }
    }
}
