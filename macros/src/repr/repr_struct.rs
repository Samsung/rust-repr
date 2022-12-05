use super::repr_type::ReprInfo;
use super::repr_util::{
    call_fields_raw_is_valid, convert_field_types_to_raw, fields_to_definition,
    ident_with_generics, impl_statement, unpack_fields, CRATE,
};
use super::ReprDeriveError;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::DataStruct;
use syn::DeriveInput;

fn struct_repr_name(def: &DeriveInput) -> syn::Ident {
    format_ident!("{}Repr", def.ident)
}

fn repr_struct(def: &DeriveInput, e: &DataStruct) -> TokenStream {
    let repr_name = struct_repr_name(def);
    let mut repr_fields = e.fields.clone();
    convert_field_types_to_raw(&mut repr_fields);
    let struct_def = fields_to_definition(&repr_name, repr_fields);

    quote! {
        #[repr(C)]
        #[derive(Clone, Copy, Debug)]
        // We're defined in a private mod. Allow re-exports.
        pub #struct_def
    }
}

fn impl_has_repr(def: &DeriveInput, e: &DataStruct) -> TokenStream {
    let e_ident = ident_with_generics(def);
    let repr_name = struct_repr_name(def);
    let impl_hasrepr = impl_statement(def, quote! { #CRATE::HasRepr }, quote! { #e_ident });
    let unpacked = unpack_fields(&repr_name, &e.fields, None);

    let checks = call_fields_raw_is_valid(&e.fields);

    quote! {
        unsafe #impl_hasrepr {
            type Raw = #repr_name;
            fn raw_is_valid(value: &#repr_name) -> Result<(), #CRATE::ReprError> {
                let #unpacked = value;
                #checks
                Ok(())
            }
        }
    }
}

pub fn repr_impl_for_struct(
    def: &DeriveInput,
    e: &DataStruct,
    info: &ReprInfo,
) -> syn::Result<TokenStream> {
    if !info.is_c {
        return Err(syn::Error::new(
            def.span(),
            ReprDeriveError::StructNeedsReprC,
        ));
    }

    let repr_struct = repr_struct(def, e);
    let impl_hasrepr = impl_has_repr(def, e);

    let out = quote! {
        #repr_struct
        #impl_hasrepr
    };
    Ok(out)
}
