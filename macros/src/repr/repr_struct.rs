use super::repr_type::ReprInfo;
use super::repr_util::{repr_impl_statement, ItemStructExt, ReprInfoExt, CRATE};
use super::ReprDeriveError;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::DeriveInput;
use syn::{DataStruct, ItemStruct};

struct GeneratedStruct<'a> {
    def: &'a DeriveInput,
    struct_def: ItemStruct,
    info: &'a ReprInfo,
}

impl<'a> GeneratedStruct<'a> {
    fn new(def: &'a DeriveInput, e: &'a DataStruct, info: &'a ReprInfo) -> Self {
        let struct_def = ItemStruct::from_fields(&format_ident!("{}Repr", def.ident), &e.fields);
        Self {
            def,
            struct_def,
            info,
        }
    }

    fn repr_type(&self) -> syn::Ident {
        format_ident!("{}Repr", self.def.ident)
    }

    fn repr_struct(&self) -> TokenStream {
        let mut struct_def = self.struct_def.clone();
        struct_def.convert_field_types_to_raw();
        let repr_attr = self.info.underlying_type_repr_attr();

        quote! {
            #repr_attr
            #[derive(Clone, Copy, Debug)]
            // We're defined in a private mod. Allow re-exports.
            pub #struct_def
        }
    }

    fn impl_has_repr(&self) -> TokenStream {
        let repr_type = self.repr_type();
        let impl_hasrepr = repr_impl_statement(self.def);

        let unpack_by_value = self.info.packed.is_some();
        let maybe_deref = if unpack_by_value { quote!(*) } else { quote!() };
        let struct_def = self.struct_def.clone();
        let unpacked = struct_def.unpack(!unpack_by_value, None);
        let checks = struct_def.call_fields_raw_is_valid(!unpack_by_value);

        quote! {
            unsafe #impl_hasrepr {
                type Raw = #repr_type;
                fn raw_is_valid(value: &#repr_type) -> Result<(), #CRATE::ReprError> {
                    let #unpacked = #maybe_deref value;
                    #checks
                    Ok(())
                }
            }
        }
    }

    fn repr_impl_for_struct(&self) -> syn::Result<TokenStream> {
        if !self.info.is_c && !self.info.is_transparent {
            return Err(syn::Error::new(
                self.def.span(),
                ReprDeriveError::StructNeedsReprC,
            ));
        }

        let repr_struct = self.repr_struct();
        let impl_hasrepr = self.impl_has_repr();

        let out = quote! {
            #repr_struct
            #impl_hasrepr
        };
        Ok(out)
    }
}

pub fn repr_impl_for_struct(
    def: &DeriveInput,
    e: &DataStruct,
    info: &ReprInfo,
) -> syn::Result<TokenStream> {
    GeneratedStruct::new(def, e, info).repr_impl_for_struct()
}
