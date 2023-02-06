use super::repr_type::ReprInfo;
use super::repr_util::{
    call_fields_raw_is_valid, convert_field_types_to_raw, fields_to_definition,
    repr_impl_statement, ReprInfoExt, unpack_fields, CRATE,
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

fn repr_struct(def: &DeriveInput, e: &DataStruct, info: &ReprInfo) -> TokenStream {
    let repr_name = struct_repr_name(def);
    let mut repr_fields = e.fields.clone();
    convert_field_types_to_raw(&mut repr_fields);
    let struct_def = fields_to_definition(&repr_name, repr_fields);
    let repr_attr = info.underlying_type_repr_attr();

    quote! {
        #repr_attr
        #[derive(Clone, Copy, Debug)]
        // We're defined in a private mod. Allow re-exports.
        pub #struct_def
    }
}

fn impl_has_repr(def: &DeriveInput, e: &DataStruct, info: &ReprInfo) -> TokenStream {
    let repr_name = struct_repr_name(def);
    let impl_hasrepr = repr_impl_statement(def);

    let unpack_by_value = info.packed.is_some();
    let maybe_deref = if unpack_by_value { quote!(*) } else { quote!() };
    let unpacked = unpack_fields(&repr_name, &e.fields, !unpack_by_value, None);
    let checks = call_fields_raw_is_valid(&e.fields, !unpack_by_value);

    quote! {
        unsafe #impl_hasrepr {
            type Raw = #repr_name;
            fn raw_is_valid(value: &#repr_name) -> Result<(), #CRATE::ReprError> {
                let #unpacked = #maybe_deref value;
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
    if !info.is_c && !info.is_transparent {
        return Err(syn::Error::new(
            def.span(),
            ReprDeriveError::StructNeedsReprC,
        ));
    }

    let repr_struct = repr_struct(def, e, info);
    let impl_hasrepr = impl_has_repr(def, e, info);

    let out = quote! {
        #repr_struct
        #impl_hasrepr
    };
    Ok(out)
}
