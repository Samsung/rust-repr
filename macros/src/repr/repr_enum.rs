use super::repr_type::ReprInfo;
use super::repr_util::{
    int_literal, repr_impl_statement, DataEnumExt, ItemStructExt, ReprInfoExt, CRATE,
};
/// Tools for deriving Repr for enums.
use super::ReprDeriveError;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{DataEnum, DeriveInput, ItemStruct};

trait EnumReprInfo {
    fn has_inner_tag(&self) -> bool;
    fn has_outer_tag(&self) -> bool;
    fn tag_name(&self) -> syn::Ident;
    fn get_enum_primitive(&self) -> syn::Result<syn::Path>;
    fn repr_c_or_transparent(&self) -> TokenStream;
}

impl EnumReprInfo for ReprInfo {
    // Transparent representation does not have a tag. It's okay to union all its fields, because
    // all ZSTs have to have alignment of 1.
    fn has_inner_tag(&self) -> bool {
        !self.is_transparent && !self.is_c
    }

    fn has_outer_tag(&self) -> bool {
        !self.is_transparent && self.is_c
    }

    fn tag_name(&self) -> syn::Ident {
        format_ident!("_repr_tag")
    }

    fn repr_c_or_transparent(&self) -> TokenStream {
        if self.is_transparent {
            quote! { transparent }
        } else {
            quote! { C }
        }
    }

    // TODO validate by enum / struct
    fn get_enum_primitive(&self) -> syn::Result<syn::Path> {
        self.primitive
            .clone()
            .ok_or_else(|| syn::Error::new(self.span, ReprDeriveError::NoPrimForEnum))
    }
}

struct GeneratedEnumVariant<'a> {
    v: &'a syn::Variant,
    info: &'a ReprInfo,
    struct_def: ItemStruct,
}

impl<'a> GeneratedEnumVariant<'a> {
    fn new(v: &'a syn::Variant, info: &'a ReprInfo) -> Self {
        let struct_def = ItemStruct::from_fields(&format_ident!("{}", v.ident), &v.fields);
        Self {
            v,
            info,
            struct_def,
        }
    }

    fn type_name(&self) -> syn::Ident {
        format_ident!("{}", self.v.ident)
    }

    fn field_name(&self) -> syn::Ident {
        format_ident!("f_{}", self.v.ident)
    }

    fn prepend_tag(&self, def: &mut ItemStruct) {
        def.prepend_field(
            &self.info.tag_name(),
            &self.info.get_enum_primitive().unwrap(),
        );
    }

    fn repr_struct(&self) -> TokenStream {
        let mut repr_struct = self.struct_def.clone();
        repr_struct.convert_field_types_to_raw();
        if self.info.has_inner_tag() {
            self.prepend_tag(&mut repr_struct);
        }
        let target_repr = self.info.repr_c_or_transparent();

        quote! {
            #[repr(#target_repr)]
            #[derive(Clone, Copy, Debug)]
            #repr_struct
        }
    }

    fn unpacked_self(&self) -> syn::Pat {
        let mut repr_struct = self.struct_def.clone();
        let mut unpack_tag = None;
        if self.info.has_inner_tag() {
            self.prepend_tag(&mut repr_struct);
            unpack_tag = Some(self.info.tag_name());
        }
        repr_struct.unpack(true, unpack_tag)
    }

    fn members_are_repr_check(&self, ident_to_unpack: &syn::Ident) -> TokenStream {
        let unpacked_self = self.unpacked_self();
        let checks = self.struct_def.call_fields_raw_is_valid(true);
        quote! {
            let #unpacked_self = #ident_to_unpack;
            #checks
        }
    }
}

struct GeneratedEnum<'a> {
    def: &'a DeriveInput,
    info: &'a ReprInfo,
    variants: Vec<GeneratedEnumVariant<'a>>,
}

impl<'a> GeneratedEnum<'a> {
    fn new(def: &'a DeriveInput, e: &'a DataEnum, info: &'a ReprInfo) -> Self {
        let variants = e
            .variants
            .iter()
            .map(|v| GeneratedEnumVariant::new(v, info))
            .collect();
        Self {
            def,
            info,
            variants,
        }
    }

    fn private_mod_name(&self) -> syn::Ident {
        format_ident!("priv_mod_{}", self.def.ident)
    }

    fn union_repr_name(&self) -> syn::Ident {
        format_ident!("{}Union", self.def.ident)
    }

    fn enum_repr_name(&self) -> syn::Ident {
        format_ident!("{}Repr", self.def.ident)
    }

    fn tag_name(&self) -> syn::Ident {
        self.info.tag_name()
    }

    fn type_name(&self) -> &syn::Ident {
        &self.def.ident
    }

    fn primitive_type(&self) -> syn::Result<syn::Type> {
        if self.info.is_transparent {
            Ok(syn::parse2(quote! { () }).unwrap())
        } else {
            let prim = self.info.get_enum_primitive()?;
            syn::parse2(quote! { #prim })
        }
    }

    // First, the easy case. No generics, since enum is fieldless.
    pub fn repr_impl_for_fieldless_enum(&self) -> syn::Result<TokenStream> {
        let prim = self.primitive_type()?;
        let e_ident = &self.type_name();

        let validation_block = if self.info.is_transparent {
            // Transparent fieldless enums are ZSTs.
            quote! { Ok(()) }
        } else {
            let names = self.variants.iter().map(|v| v.type_name());
            quote! {
                match value {
                    #(x if *x == #e_ident::#names as #prim => Ok(())),*,
                    _ => Err(#CRATE::ReprError),
                }
            }
        };

        let out = quote! {
            unsafe impl #CRATE::HasRepr for #e_ident {
                type Raw = #prim;

                fn raw_is_valid(value: &#prim) -> Result<(), #CRATE::ReprError> {
                    #validation_block
                }
            }
        };
        Ok(out)
    }

    fn repr_structs(&self) -> TokenStream {
        let repr_attr = self.info.underlying_type_repr_attr();
        let repr_name = self.enum_repr_name();
        let union_name = self.union_repr_name();
        let structs = self.variants.iter().map(|v| v.repr_struct());

        if self.info.is_transparent {
            let single_variant_type = self.variants[0].type_name();
            // Remove the union, since repr(transparent) doesn't support it. The reason we don't
            // want repr(C) is to keep the same ABI between the original type and the underlying
            // type.
            return quote! {
                #(#structs)*

                #repr_attr
                #[derive(Clone, Copy, Debug)]
                pub struct #repr_name(#single_variant_type);
            };
        }

        let tag_type = self.info.get_enum_primitive().unwrap();
        let tag_member = self.tag_name();
        let tag_member = quote! { #tag_member: #tag_type,};

        let (inner_tag, outer_tag) = if self.info.has_inner_tag() {
            (tag_member, quote! {})
        } else {
            (quote! {}, tag_member)
        };
        let union_members = self.variants.iter().map(|v| {
            let name = v.type_name();
            let field_name = v.field_name();
            quote! { #field_name: #name }
        });

        quote! {
            #(#structs)*

            #[repr(C)]
            #[derive(Clone, Copy)]
            #[allow(non_snake_case)]
            union #union_name {
                #inner_tag
                #(#union_members,)*
            }

            #repr_attr
            #[derive(Clone, Copy)]
            pub struct #repr_name {  // We're defined in a private mod. Allow re-exports.
                #outer_tag
                u: #union_name,
            }
        }
    }

    fn impl_repr_structs(&self) -> TokenStream {
        let impl_hasrepr = self.impl_has_repr();
        let impl_debug = if !self.info.is_transparent {
            self.union_repr_impl_debug()
        } else {
            quote! {}
        };

        quote! {
            #impl_hasrepr
            #impl_debug
        }
    }

    fn access_tag_member(&self, val: &syn::Ident) -> TokenStream {
        let tag_member = self.tag_name();
        if self.info.is_c {
            quote! { #val.#tag_member }
        } else {
            quote! { unsafe { #val.u.#tag_member }}
        }
    }

    fn impl_has_repr(&self) -> TokenStream {
        let repr_var = format_ident!("value");
        let variant_var = format_ident!("variant");
        let variant_members_are_repr_check = self
            .variants
            .iter()
            .map(|v| v.members_are_repr_check(&variant_var));
        let raw_is_valid_body = if self.info.is_transparent {
            quote! {
                let #variant_var = #repr_var.0;
                #(#variant_members_are_repr_check)*
                Ok(())
            }
        } else {
            let get_tag = self.access_tag_member(&repr_var);
            let tag_literals = (0..self.variants.len()).map(|i| int_literal(i, self.def.span()));
            let variant_field_names = self.variants.iter().map(|v| v.field_name());
            quote! {
                match #get_tag {
                    #(
                        #tag_literals => {
                            let #variant_var = unsafe { &#repr_var.u.#variant_field_names };
                            #variant_members_are_repr_check
                            Ok(())
                        },
                    )*
                        _ => Err(#CRATE::ReprError),
                }
            }
        };

        let repr_name = self.enum_repr_name();
        let impl_hasrepr = repr_impl_statement(self.def);
        quote! {
            unsafe #impl_hasrepr {
                type Raw = #repr_name;

                fn raw_is_valid(#repr_var: &#repr_name) -> Result<(), #CRATE::ReprError> {
                    #raw_is_valid_body
                }
            }
        }
    }

    fn union_repr_impl_debug(&self) -> TokenStream {
        let repr_name = self.enum_repr_name();
        let repr_var = format_ident!("self");
        let tag_member = self.tag_name();
        let get_tag = self.access_tag_member(&repr_var);
        let tag_literals = (0..self.variants.len()).map(|i| int_literal(i, self.def.span()));
        let variant_field_names = self.variants.iter().map(|v| v.field_name());

        quote! {
            impl core::fmt::Debug for #repr_name {
                fn fmt(&self, ff: &mut core::fmt::Formatter) -> Result<(), core::fmt::Error> {
                    let mut ds = ff.debug_struct(stringify!(#repr_name));
                    let mut fmt = ds.field(stringify!(#tag_member), &#get_tag);
                    let fmt = match #get_tag {
                        #(
                            #tag_literals => {
                                let m = unsafe { self.u.#variant_field_names };
                                fmt.field(stringify!(#variant_field_names), &m)
                            },
                        )*
                            _ => fmt,
                    };
                    fmt.finish()
                }
            }
        }
    }

    fn repr_impl_for_enum_with_fields(&self) -> syn::Result<TokenStream> {
        // There are unstable extensions that allow setting values. Let's explicitly disallow that.
        let structs = self.repr_structs();
        let impls = self.impl_repr_structs();

        let mod_name = self.private_mod_name();
        let repr_name = self.enum_repr_name();
        let vis = &self.def.vis;

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
}

pub fn repr_impl_for_enum(
    def: &DeriveInput,
    e: &DataEnum,
    info: &ReprInfo,
) -> syn::Result<TokenStream> {
    if e.is_empty() {
        return Err(syn::Error::new(def.span(), ReprDeriveError::EnumIsEmpty));
    }
    // If enum is transparent, it's allowed not to have a primitive specified.
    if !info.is_transparent {
        let _ = info.get_enum_primitive()?;
    }
    e.check_specifies_discriminants_like_repr_wants(&info)?;

    if e.is_fieldless() {
        GeneratedEnum::new(def, e, info).repr_impl_for_fieldless_enum()
    } else {
        GeneratedEnum::new(def, e, info).repr_impl_for_enum_with_fields()
    }
}
