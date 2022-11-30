use self::repr_enum::repr_impl_for_enum;
use self::repr_struct::repr_impl_for_struct;
use self::repr_type::get_repr_type;
use self::repr_util::def_has_non_lifetime_generics;
use proc_macro2::TokenStream;
use syn::spanned::Spanned;

pub(crate) mod repr_enum;
pub(crate) mod repr_struct;
pub(crate) mod repr_type;
pub(crate) mod repr_util;

pub(crate) mod err {
    pub const MULT_PRIM: &str = "Multiple primitives in repr";
    pub const MULT_ALIGN: &str = "Multiple align() in repr";
    pub const MULT_PACKED: &str = "Multiple packed() in repr";
    pub const INVALID_PACKED_ALIGN: &str = "Unexpected packed/align contents";
    pub const UNEXPECTED_REPR: &str = "Unexpected repr item";
    pub const NO_PRIM_FOR_ENUM: &str = "Enum does not specify a fixed repr type";
    pub const ENUM_IS_EMPTY: &str = "Enum cannot be empty";
    pub const FIELDLESS_ENUM_NEEDS_ALL_VALUES: &str =
        "Fieldless enum should specify values for all variants";
    pub const FIELD_ENUM_CANNOT_SET_VALUES: &str =
        "Enum with fields cannot specify values for variants";
    pub const PACKED_NOT_SUPPORTED: &str = "Packed structs are not supported yet";
    pub const ALIGN_NOT_SUPPORTED: &str = "Aligned structs/enums are not supported yet";
    pub const NON_LIFETIME_GENERICS_NOT_SUPPORTED: &str = "Non-lifetime generics are not supported";
    pub const STRUCT_NEEDS_REPR_C: &str = "Repr for structs needs repr(C)";
    pub const NO_REPR_FOR_UNION: &str = "Repr can't be derived for unions";
}

pub fn do_derive(input: TokenStream) -> syn::Result<TokenStream> {
    let d = syn::parse2(input)?;
    if def_has_non_lifetime_generics(&d) {
        return Err(syn::Error::new(
            d.span(),
            err::NON_LIFETIME_GENERICS_NOT_SUPPORTED,
        ));
    }

    let info = get_repr_type(&d)?;

    if info.packed.is_some() {
        return Err(syn::Error::new(d.span(), err::PACKED_NOT_SUPPORTED));
    }
    if info.align.is_some() {
        return Err(syn::Error::new(d.span(), err::ALIGN_NOT_SUPPORTED));
    }

    match &d.data {
        syn::Data::Enum(e) => repr_impl_for_enum(&d, e, &info),
        syn::Data::Struct(s) => repr_impl_for_struct(&d, s, &info),
        _ => Err(syn::Error::new(d.span(), err::NO_REPR_FOR_UNION)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use quote::quote;

    fn has_err<T: core::fmt::Debug>(e: syn::Result<T>, s: &'static str) -> bool {
        format!("{}", e.unwrap_err()).contains(s)
    }

    #[test]
    fn test_struct_no_repr() {
        let s = quote! {
            enum Foo {
                BAR = 5,
            }
        };
        assert!(has_err(do_derive(s), err::NO_PRIM_FOR_ENUM));
    }

    #[test]
    fn test_struct_multiple_repr() {
        let s = quote! {
            #[repr(C)]
            #[repr(u8)]
            enum Foo {
                BAR = 5,
            }
        };
        do_derive(s).unwrap();
    }

    #[test]
    fn test_struct_invalid_repr() {
        let s = quote! {
            #[repr(transparent)]
            struct Foo;
        };
        assert!(has_err(do_derive(s), err::UNEXPECTED_REPR));
    }

    #[test]
    fn test_struct_multiple_prim() {
        let s = quote! {
            #[repr(C, u8)]
            #[repr(u16)]
            enum Foo {
                BAR = 0,
            }
        };
        assert!(has_err(do_derive(s), err::MULT_PRIM));
    }

    #[test]
    fn test_struct_multiple_align() {
        let s = quote! {
            #[repr(C, align(4), align(16))]
            struct Foo;
        };
        assert!(has_err(do_derive(s), err::MULT_ALIGN));
    }

    #[test]
    fn test_struct_multiple_packed() {
        let s = quote! {
            #[repr(C, packed, packed(2))]
            struct Foo;
        };
        assert!(has_err(do_derive(s), err::MULT_PACKED));
    }

    #[test]
    fn test_struct_invalid_packed_align() {
        let s = quote! {
            #[repr(C, packed("a"))]
            struct Foo;
        };
        assert!(has_err(do_derive(s), err::INVALID_PACKED_ALIGN));
    }

    #[test]
    fn test_struct_bare_packed() {
        let s = quote! {
            #[repr(C, packed)]
            struct Foo;
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), err::PACKED_NOT_SUPPORTED));
    }

    #[test]
    fn test_struct_align() {
        let s = quote! {
            #[repr(C, align(4))]
            struct Foo;
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), err::ALIGN_NOT_SUPPORTED));
    }

    #[test]
    fn test_struct_no_repr_c() {
        let s = quote! {
            struct Foo;
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), err::STRUCT_NEEDS_REPR_C));
    }

    #[test]
    fn test_no_repr_for_union() {
        let s = quote! {
            union Foo {
                x: u8,
                y: u16,
            }
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), err::NO_REPR_FOR_UNION));
    }

    #[test]
    fn test_enum_fixed_repr() {
        let s = quote! {
            #[repr(u16)]
            enum Foo {
                BAR = 0,
            }
        };
        do_derive(s).unwrap();
    }

    #[test]
    fn test_enum_with_no_prim() {
        let s = quote! {
            #[repr(C)]
            enum Foo {
                BAR = 0,
            }
        };
        assert!(has_err(do_derive(s), err::NO_PRIM_FOR_ENUM));
    }

    #[test]
    fn test_enum_with_missing_values() {
        let s = quote! {
            #[repr(u8)]
            enum Foo {
                BAR = 0,
                BAZ,
            }
        };
        assert!(has_err(do_derive(s), err::FIELDLESS_ENUM_NEEDS_ALL_VALUES));
    }

    #[test]
    fn test_enum_with_values_and_fields() {
        let s = quote! {
            #[repr(u8)]
            enum Foo {
                BAR(usize) = 0,
                BAZ = 1,
            }
        };
        assert!(has_err(do_derive(s), err::FIELD_ENUM_CANNOT_SET_VALUES));
    }

    #[test]
    fn test_no_support_for_non_lifetime_generics() {
        let s = quote! {
            #[repr(C)]
            struct Foo<T> {
                x: T,
            }
        };
        assert!(has_err(
            do_derive(s),
            err::NON_LIFETIME_GENERICS_NOT_SUPPORTED
        ));
    }
}
