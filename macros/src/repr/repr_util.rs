//! Various macro utilities.

use super::{repr_type::ReprInfo, ReprDeriveError};
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
pub const CRATE: Quote = Quote("::repr");

pub fn def_has_non_lifetime_generics(def: &DeriveInput) -> bool {
    def.generics
        .params
        .iter()
        .any(|v| !matches!(v, syn::GenericParam::Lifetime(_)))
}

#[cfg(test)]
#[test]
fn test_def_has_non_lifetime_generics() {
    for (s, res) in [
        ("struct Foo {}", false),
        ("struct Foo<'a> {}", false),
        ("struct Foo<T> {}", true),
        ("struct Foo<const N: usize> {}", true),
        ("struct Foo<'a, T> {}", true),
        ("struct Foo<'a, const N: usize> {}", true),
    ] {
        let d: DeriveInput = syn::parse_str(s).unwrap();
        assert_eq!(def_has_non_lifetime_generics(&d), res);
    }
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
                ReprDeriveError::FieldlessEnumNeedsAllValues,
            ));
        }
    }
    Ok(())
}

pub fn enum_should_have_no_discriminants(e: &DataEnum) -> syn::Result<()> {
    for v in e.variants.iter() {
        if v.discriminant.is_some() {
            return Err(syn::Error::new(
                v.span(),
                ReprDeriveError::FieldEnumCannotSetValues,
            ));
        }
    }
    Ok(())
}

// Unpack structs and tuples into variables <_0, _1, ...>.
// Optionally assign a custom name to first variable, then start counting from 0.
// The 'ref' argument controls whether the unpacked fields are references or copies. This is
// relevant when unpacking a packed struct/enum.
pub fn unpack_fields(
    ty: &syn::Ident,
    fields: &Fields,
    ref_: bool,
    first_name: Option<syn::Ident>,
) -> syn::Pat {
    let count = if first_name.is_some() {
        fields.len() - 1
    } else {
        fields.len()
    };

    let idents = first_name
        .into_iter()
        .chain((0..count).map(|i| format_ident!("_{}", i)));

    let maybe_ref = if ref_ { quote!(ref) } else { quote!() };

    let raw = match fields {
        Fields::Named(fs) => {
            let fields = fs.named.iter().map(|i| i.ident.as_ref().unwrap());
            quote! {
                #ty { #(#fields: #maybe_ref #idents),* }
            }
        }
        Fields::Unnamed(_) => {
            quote! { #ty (#(#maybe_ref #idents),*) }
        }
        _ => quote! { #ty },
    };
    syn::parse2(raw).unwrap()
}

#[cfg(test)]
#[test]
fn test_unpack_fields() {
    let ident = format_ident!("Foo");
    for (s, ref_, first_name, expect) in [
        (
            "struct Foo(bool, u64, u8);",
            true,
            None,
            "Foo(ref _0 , ref _1 , ref _2)",
        ),
        (
            "struct Foo{a: bool, b: u64, c: u8}",
            true,
            None,
            "Foo{a: ref _0, b: ref _1, c: ref _2}",
        ),
        (
            "struct Foo(bool, u64, u8);",
            true,
            Some(format_ident!("foo")),
            "Foo(ref foo, ref _0, ref _1)",
        ),
        (
            "struct Foo{a: bool, b: u64, c: u8}",
            true,
            Some(format_ident!("foo")),
            "Foo{a: ref foo, b: ref _0, c: ref _1}",
        ),
        ("struct Foo;", true, None, "Foo"),
        (
            "struct Foo(bool, u64, u8);",
            false,
            None,
            "Foo(_0 , _1 , _2)",
        ),
    ] {
        let f: syn::ItemStruct = syn::parse_str(s).unwrap();
        let expect: syn::Pat = syn::parse_str(expect).unwrap();
        let pat = unpack_fields(&ident, &f.fields, ref_, first_name);
        assert_eq!(pat, expect);
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

#[cfg(test)]
#[test]
fn test_prepend_field() {
    let ident = format_ident!("foo");
    let ty: syn::Path = syn::parse_str("baz::Bar").unwrap();
    for (s, expect) in [
        (
            "struct Foo(bool, u64, u8);",
            "struct Foo(baz::Bar, bool, u64, u8);",
        ),
        (
            "struct Foo{a: bool, b: u64}",
            "struct Foo{foo: baz::Bar, a: bool, b: u64}",
        ),
        ("struct Foo;", "struct Foo(baz::Bar);"),
    ] {
        let mut f: syn::ItemStruct = syn::parse_str(s).unwrap();
        let expect: syn::ItemStruct = syn::parse_str(expect).unwrap();
        prepend_field(&mut f.fields, &ident, &ty);
        assert!(f == expect);
    }
}

// Turn a fields struct into struct / enum definition, with a semicolon if needed.
pub fn fields_to_definition(name: &syn::Ident, fields: Fields) -> syn::ItemStruct {
    let delim = match fields {
        Fields::Named(_) => quote! {},
        _ => quote! {;},
    };
    syn::parse2(quote! { struct #name #fields #delim }).unwrap()
}

// Turn usize into int literal without suffix.
pub fn int_literal(lit: usize, span: proc_macro2::Span) -> syn::Lit {
    syn::LitInt::new(&lit.to_string(), span).into()
}

// Convert types of all fields to whatever's returned by f.
fn convert_field_types(fields: &mut Fields, f: &mut dyn FnMut(TokenStream) -> TokenStream) {
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
// _ref specifies whether the fields are references or not.
pub fn call_fields_raw_is_valid(fields: &Fields, _ref: bool) -> TokenStream {
    let mut fd = fields.clone();
    statify_lifetimes(&mut fd);
    convert_field_types_to_repr(&mut fd);

    let maybe_ref = if _ref { quote!() } else { quote!(&) };

    let idents = (0..fields.len()).map(|i| format_ident!("_{}", i));
    let types = fd.iter().map(|f| &f.ty);
    quote! {
        #(#types::raw_is_valid(#maybe_ref #idents)?;)*
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

// Generate a repr impl statement, taking into account generics.
pub fn repr_impl_statement(def: &DeriveInput) -> TokenStream {
    let ident = &def.ident;

    if def.generics.params.is_empty() {
        quote! { impl #CRATE::HasRepr for #ident }
    } else {
        let generics = &def.generics.params;
        let where_clause = &def.generics.where_clause;
        quote! { impl<#generics> #CRATE::HasRepr for #ident<#generics> #where_clause }
    }
}

pub fn underlying_type_repr_attr(info: &ReprInfo) -> TokenStream {
    let mut repr_items: Vec<TokenStream> = vec![];
    if info.is_transparent {
        repr_items.push(quote!(transparent));
    } else {
        repr_items.push(quote!(C));
    }

    if let Some(a) = &info.align {
        repr_items.push(quote!(align(#a)));
    }
    if let Some(a) = &info.packed {
        repr_items.push(quote!(packed(#a)));
    }
    quote!(#[repr(#(#repr_items),*)])
}
