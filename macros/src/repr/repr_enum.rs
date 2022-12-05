use super::repr_type::ReprInfo;
use super::repr_util::{
    call_fields_raw_is_valid, convert_field_types_to_raw, enum_is_empty, enum_is_fieldless,
    enum_should_have_all_discriminants, enum_should_have_no_discriminants, fields_to_body,
    ident_with_generics, impl_statement, int_literal, prepend_field, unpack_fields, CRATE,
};
/// Tools for deriving Repr for enums.
use super::ReprDeriveError;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{DataEnum, DeriveInput, Variant};

// First, the easy case. No generics, since enum is fieldless.
pub fn repr_impl_for_fieldless_enum(
    def: &DeriveInput,
    e: &DataEnum,
    info: &ReprInfo,
) -> syn::Result<TokenStream> {
    let prim = info.get_enum_primitive()?;
    enum_should_have_all_discriminants(e)?;

    let e_ident = &def.ident;

    let try_from_match = e.variants.iter().map(|v| {
        let disc = &v.discriminant.as_ref().unwrap().1;
        quote! { #disc => Ok(()) }
    });

    let out = quote! {
        unsafe impl #CRATE::HasRepr for #e_ident {
            type Raw = #prim;

            fn raw_is_valid(value: &#prim) -> Result<(), #CRATE::ReprError> {
                match value {
                    #(#try_from_match),*,
                    _ => Err(#CRATE::ReprError),
                }
            }
        }
    };
    Ok(out)
}

// Various derived names and types.
fn private_mod_name(def: &DeriveInput) -> syn::Ident {
    format_ident!("priv_mod_{}", def.ident)
}

fn variant_repr_name(variant: &syn::Variant) -> syn::Ident {
    format_ident!("{}", variant.ident)
}

fn variant_field_name(variant: &syn::Variant) -> syn::Ident {
    format_ident!("f_{}", variant.ident)
}

fn union_repr_name(def: &DeriveInput) -> syn::Ident {
    format_ident!("{}Union", def.ident)
}

fn enum_repr_name(def: &DeriveInput) -> syn::Ident {
    format_ident!("{}Repr", def.ident)
}

fn tag_member_name() -> syn::Ident {
    format_ident!("_repr_tag")
}

// Prepend tag to struct fields. Used for non-repr(C) enums, where tag is part of each struct.
fn prepend_tag(fields: &mut syn::Fields, info: &ReprInfo) {
    prepend_field(
        fields,
        &tag_member_name(),
        info.get_enum_primitive().unwrap(),
    );
}

fn repr_struct_for_variant(variant: &syn::Variant, info: &ReprInfo) -> TokenStream {
    let repr_name = variant_repr_name(variant);

    let mut repr_fields = variant.fields.clone();
    convert_field_types_to_raw(&mut repr_fields);
    if !info.is_c {
        prepend_tag(&mut repr_fields, info);
    }
    let repr_fields = fields_to_body(repr_fields);

    quote! {
        #[repr(C)]
        #[derive(Clone, Copy, Debug)]
        struct #repr_name #repr_fields
    }
}

fn repr_structs(def: &DeriveInput, e: &DataEnum, info: &ReprInfo) -> TokenStream {
    let tag_member = tag_member_name();
    let tag_type = info.get_enum_primitive().unwrap();
    let tag_member = quote! { #tag_member: #tag_type, };
    let (outer_tag, inner_tag) = if info.is_c {
        (tag_member, quote! {})
    } else {
        (quote! {}, tag_member)
    };

    let structs = e.variants.iter().map(|v| repr_struct_for_variant(v, info));
    let union_members = e.variants.iter().map(|v| {
        let name = variant_repr_name(v);
        let field_name = variant_field_name(v);
        quote! { #field_name: #name }
    });
    let union_name = union_repr_name(def);
    let repr_name = enum_repr_name(def);

    // TODO support non-repr(C) enums as well.
    quote! {
        #(#structs)*

        #[repr(C)]
        #[derive(Clone, Copy)]
        #[allow(non_snake_case)]
        union #union_name {
            #inner_tag
            #(#union_members,)*
        }

        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct #repr_name {  // We're defined in a private mod. Allow re-exports.
            #outer_tag
            u: #union_name,
        }
    }
}

fn unpacked_variant_repr(variant: &Variant, info: &ReprInfo) -> syn::Pat {
    let mut repr_fields = variant.fields.clone();
    let mut unpack_tag = None;
    if !info.is_c {
        prepend_tag(&mut repr_fields, info);
        unpack_tag = Some(tag_member_name());
    }
    let variant_repr_name = variant_repr_name(variant);
    unpack_fields(&variant_repr_name, &repr_fields, unpack_tag)
}

// Produces a match branch that calls `raw_is_valid` on all variant members.
fn check_variant(
    variant: &Variant,
    repr_var: &syn::Ident,
    info: &ReprInfo,
    tag: usize,
) -> TokenStream {
    let unpacked_variant_repr = unpacked_variant_repr(variant, info);

    let checks = call_fields_raw_is_valid(&variant.fields);
    let union_field_name = variant_field_name(variant);
    let tag_literal = int_literal(tag, repr_var.span());

    quote! {
        #tag_literal => {
            let #unpacked_variant_repr = unsafe { #repr_var.u.#union_field_name };
            #checks
            Ok(())
        }
    }
}

fn repr_to_debug(variant: &Variant, fmt_var: &syn::Ident, tag: usize) -> TokenStream {
    let union_field_name = variant_field_name(variant);
    let tag_literal = int_literal(tag, fmt_var.span());

    quote! {
        #tag_literal => {
            let m = unsafe { self.u.#union_field_name };
            #fmt_var.field(stringify!(#union_field_name), &m)
        }
    }
}

fn access_tag_member(info: &ReprInfo, val: &syn::Ident) -> TokenStream {
    let tag_member = tag_member_name();
    if info.is_c {
        quote! { #val.#tag_member }
    } else {
        quote! { unsafe { #val.u.#tag_member }}
    }
}

fn impl_has_repr(def: &DeriveInput, e: &DataEnum, info: &ReprInfo) -> TokenStream {
    let e_ident = ident_with_generics(def);
    let repr_name = enum_repr_name(def);
    let impl_hasrepr = impl_statement(def, quote! { #CRATE::HasRepr }, quote! { #e_ident });

    let repr_var = format_ident!("value");
    let get_tag = access_tag_member(info, &repr_var);
    let validate_branches = e
        .variants
        .iter()
        .enumerate()
        .clone()
        .map(|(i, v)| check_variant(v, &repr_var, info, i));

    quote! {
        unsafe #impl_hasrepr {
            type Raw = #repr_name;

            fn raw_is_valid(#repr_var: &#repr_name) -> Result<(), #CRATE::ReprError> {
                match #get_tag {
                    #(#validate_branches,)*
                    _ => Err(#CRATE::ReprError),
                }
            }
        }
    }
}

fn impl_debug(def: &DeriveInput, e: &DataEnum, info: &ReprInfo) -> TokenStream {
    let repr_name = enum_repr_name(def);

    let repr_var = format_ident!("value");
    let fmt_var = format_ident!("fmt");
    let tag_member = tag_member_name();
    let get_tag = access_tag_member(info, &repr_var);
    let debug_branches = e
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| repr_to_debug(v, &fmt_var, i));

    quote! {
        impl core::fmt::Debug for #repr_name {
            fn fmt(&self, ff: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
                let #repr_var = self;

                let mut ds = ff.debug_struct(stringify!(#repr_name));
                let mut #fmt_var = ds.field(stringify!(#tag_member), &#get_tag);
                let #fmt_var = match #get_tag {
                    #(#debug_branches,)*
                    _ => #fmt_var,
                };
                #fmt_var.finish()
            }
        }
    }
}

fn impl_repr_structs(def: &DeriveInput, e: &DataEnum, info: &ReprInfo) -> TokenStream {
    let impl_hasrepr = impl_has_repr(def, e, info);
    let impl_debug = impl_debug(def, e, info);

    quote! {
        #impl_hasrepr
        #impl_debug
    }
}

fn repr_impl_for_enum_with_fields(
    def: &DeriveInput,
    e: &DataEnum,
    info: &ReprInfo,
) -> syn::Result<TokenStream> {
    // There are unstable extensions that allow setting values. Let's explicitly disallow that.
    enum_should_have_no_discriminants(e)?;
    let structs = repr_structs(def, e, info);
    let impls = impl_repr_structs(def, e, info);

    let mod_name = private_mod_name(def);
    let repr_name = enum_repr_name(def);
    let vis = &def.vis;

    let out = quote! {
        #[allow(non_snake_case)]
        mod #mod_name {
            use super::*;

            #structs
            #impls
        }
        // If necessary, export repr type.
        #[allow(unused_imports)]
        #vis use #mod_name::#repr_name;
    };
    Ok(out)
}

pub fn repr_impl_for_enum(
    def: &DeriveInput,
    e: &DataEnum,
    info: &ReprInfo,
) -> syn::Result<TokenStream> {
    if enum_is_empty(e) {
        return Err(syn::Error::new(def.span(), ReprDeriveError::EnumIsEmpty));
    }
    let _ = info.get_enum_primitive()?;

    if enum_is_fieldless(e) {
        repr_impl_for_fieldless_enum(def, e, info)
    } else {
        repr_impl_for_enum_with_fields(def, e, info)
    }
}
