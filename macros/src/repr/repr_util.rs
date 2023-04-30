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
pub const CRATE: Quote = Quote("::isrepr");

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

pub trait DataEnumExt {
    fn is_empty(&self) -> bool;
    fn is_fieldless(&self) -> bool;
    fn check_specifies_discriminants_like_repr_wants(&self, info: &ReprInfo) -> syn::Result<()>;
}

impl DataEnumExt for DataEnum {
    fn is_empty(&self) -> bool {
        self.variants.iter().next().is_none()
    }

    fn is_fieldless(&self) -> bool {
        self.variants
            .iter()
            .all(|v| matches!(v.fields, syn::Fields::Unit))
    }

    fn check_specifies_discriminants_like_repr_wants(&self, info: &ReprInfo) -> syn::Result<()> {
        if info.is_transparent {
            // repr(transparent) will complain for us
            return Ok(());
        }

        let is_fieldless = self.is_fieldless();
        let err = |v: &syn::Variant, e| syn::Error::new(v.span(), e);
        for v in self.variants.iter() {
            if is_fieldless && v.discriminant.is_none() {
                return Err(err(v, ReprDeriveError::FieldlessEnumNeedsAllValues));
            } else if !is_fieldless && v.discriminant.is_some() {
                return Err(err(v, ReprDeriveError::FieldEnumCannotSetValues));
            }
        }
        Ok(())
    }
}

trait ItemStructExtPriv {
    fn statify_lifetimes(&mut self);
    fn convert_field_types(&mut self, f: &mut dyn FnMut(TokenStream) -> TokenStream);
}

pub trait ItemStructExt: Sized {
    fn from_fields(ty: &syn::Ident, fields: &Fields) -> Self;
    fn unpack(&self, ref_: bool, first_name: Option<syn::Ident>) -> syn::Pat;
    fn prepend_field(&mut self, id: &syn::Ident, ty: &syn::Path);
    fn convert_field_types_to_repr(&mut self);
    fn convert_field_types_to_raw(&mut self);
    fn call_fields_raw_is_valid(&self, _ref: bool) -> TokenStream;
}

impl ItemStructExtPriv for syn::ItemStruct {
    fn statify_lifetimes(&mut self) {
        StatifyLifetimes.visit_item_struct_mut(self)
    }

    // Convert types of all fields to whatever's returned by f.
    fn convert_field_types(&mut self, f: &mut dyn FnMut(TokenStream) -> TokenStream) {
        let mut change_type = |fd: &mut syn::Field| {
            let ty = &fd.ty;
            fd.ty = syn::Type::Verbatim(f(quote! { #ty }));
        };

        match &mut self.fields {
            Fields::Named(f) => {
                for field in f.named.iter_mut() {
                    change_type(field);
                }
            }
            Fields::Unnamed(f) => {
                for field in f.unnamed.iter_mut() {
                    change_type(field);
                }
            }
            _ => (),
        }
    }
}

impl ItemStructExt for syn::ItemStruct {
    fn from_fields(ty: &syn::Ident, fields: &Fields) -> Self {
        let delim = match fields {
            Fields::Named(_) => quote! {},
            _ => quote! {;},
        };
        syn::parse2(quote! { struct #ty #fields #delim }).unwrap()
    }

    // Unpack structs and tuples into variables <_0, _1, ...>.
    // Optionally assign a custom name to first variable, then start counting from 0.
    // The 'ref' argument controls whether the unpacked fields are references or copies. This is
    // relevant when unpacking a packed struct/enum.
    fn unpack(&self, ref_: bool, first_name: Option<syn::Ident>) -> syn::Pat {
        let count = if first_name.is_some() {
            self.fields.len() - 1
        } else {
            self.fields.len()
        };

        let idents = first_name
            .into_iter()
            .chain((0..count).map(|i| format_ident!("_{}", i)));

        let maybe_ref = if ref_ { quote!(ref) } else { quote!() };
        let ty = &self.ident;
        let fields = &self.fields;

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

    // Prepend a field to a list of fields.
    fn prepend_field(&mut self, id: &syn::Ident, ty: &syn::Path) {
        // Everything parsed here should be valid.
        match &mut self.fields {
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
                self.fields = f.into();
            }
        }
    }

    fn convert_field_types_to_repr(&mut self) {
        self.convert_field_types(&mut |ty| quote! { <#ty as #CRATE::HasRepr> })
    }

    // Also converts lifetimes to static, since Raw types should stay the same.
    fn convert_field_types_to_raw(&mut self) {
        self.statify_lifetimes();
        self.convert_field_types(&mut |ty| quote! { <#ty as #CRATE::HasRepr>::Raw })
    }

    // Call raw_is_valid for unpacked fields <_0, _1, ...>.
    // _ref specifies whether the fields are references or not.
    fn call_fields_raw_is_valid(&self, _ref: bool) -> TokenStream {
        let mut fd = self.clone();
        fd.statify_lifetimes();
        fd.convert_field_types_to_repr();
        let fd = fd.fields;

        let maybe_ref = if _ref { quote!() } else { quote!(&) };

        let idents = (0..fd.len()).map(|i| format_ident!("_{}", i));
        let types = fd.iter().map(|f| &f.ty);
        quote! {
            #(#types::raw_is_valid(#maybe_ref #idents)?;)*
        }
    }
}

#[cfg(test)]
#[test]
fn test_unpack_fields() {
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
        let pat = f.unpack(ref_, first_name);
        assert_eq!(pat, expect);
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
        f.prepend_field(&ident, &ty);
        assert!(f == expect);
    }
}

// Turn usize into int literal without suffix.
pub fn int_literal(lit: usize, span: proc_macro2::Span) -> syn::Lit {
    syn::LitInt::new(&lit.to_string(), span).into()
}

pub struct StatifyLifetimes;
impl syn::visit_mut::VisitMut for StatifyLifetimes {
    fn visit_lifetime_mut(&mut self, l: &mut syn::Lifetime) {
        l.ident = syn::Ident::new("static", Span::call_site());
        syn::visit_mut::visit_lifetime_mut(self, l)
    }
}

pub trait ReprInfoExt {
    fn underlying_type_repr_attr(&self) -> TokenStream;
}

impl ReprInfoExt for ReprInfo {
    fn underlying_type_repr_attr(&self) -> TokenStream {
        let mut repr_items: Vec<TokenStream> = vec![];
        if self.is_transparent {
            repr_items.push(quote!(transparent));
        } else {
            repr_items.push(quote!(C));
        }

        if let Some(a) = &self.align {
            repr_items.push(quote!(align(#a)));
        }
        if let Some(a) = &self.packed {
            repr_items.push(quote!(packed(#a)));
        }
        quote!(#[repr(#(#repr_items),*)])
    }
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
