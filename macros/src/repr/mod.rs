use std::fmt::Display;
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

#[derive(PartialEq, Eq)]
enum ReprDeriveError {
    MultiplePrimitives,
    MultipleAlign,
    MultiplePacked,
    InvalidPackedAlign,
    UnexpectedRepr,
    NoPrimForEnum,
    EnumIsEmpty,
    FieldlessEnumNeedsAllValues,
    FieldEnumCannotSetValues,
    PackedNotSupported,
    AlignNotSupported,
    NonLifetimeGenericsNotSupported,
    StructNeedsReprC,
    NoReprForUnion,
}

impl Display for ReprDeriveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            Self::MultiplePrimitives => "Multiple primitives in repr",
            Self::MultipleAlign => "Multiple align() in repr",
            Self::MultiplePacked => "Multiple packed() in repr",
            Self::InvalidPackedAlign => "Unexpected packed/align contents",
            Self::UnexpectedRepr => "Unexpected repr item",
            Self::NoPrimForEnum => "Enum does not specify a fixed repr type",
            Self::EnumIsEmpty => "Enum cannot be empty",
            Self::FieldlessEnumNeedsAllValues => "Fieldless enum should specify values for all variants",
            Self::FieldEnumCannotSetValues => "Enum with fields cannot specify values for variants",
            Self::PackedNotSupported => "Packed structs are not supported yet",
            Self::AlignNotSupported => "Aligned structs/enums are not supported yet",
            Self::NonLifetimeGenericsNotSupported => "Non-lifetime generics are not supported",
            Self::StructNeedsReprC => "Repr for structs needs repr(C)",
            Self::NoReprForUnion => "Repr can't be derived for unions",
        };
        write!(f, "{}", msg)
    }
}

pub fn do_derive(input: TokenStream) -> syn::Result<TokenStream> {
    let d = syn::parse2(input)?;
    if def_has_non_lifetime_generics(&d) {
        return Err(syn::Error::new(
            d.span(),
            ReprDeriveError::NonLifetimeGenericsNotSupported
        ));
    }

    let info = get_repr_type(&d)?;

    if info.packed.is_some() {
        return Err(syn::Error::new(d.span(),
        ReprDeriveError::PackedNotSupported));
    }
    if info.align.is_some() {
        return Err(syn::Error::new(d.span(), ReprDeriveError::AlignNotSupported));
    }

    match &d.data {
        syn::Data::Enum(e) => repr_impl_for_enum(&d, e, &info),
        syn::Data::Struct(s) => repr_impl_for_struct(&d, s, &info),
        _ => Err(syn::Error::new(d.span(), ReprDeriveError::NoReprForUnion)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use quote::quote;

    fn has_err<T: core::fmt::Debug>(e: syn::Result<T>, s: ReprDeriveError) -> bool {
        // The best we can do with syn::Error.
        e.unwrap_err().to_string() == s.to_string()
    }

    #[test]
    fn test_struct_no_repr() {
        let s = quote! {
            enum Foo {
                BAR = 5,
            }
        };
        assert!(has_err(do_derive(s), ReprDeriveError::NoPrimForEnum));
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
        assert!(has_err(do_derive(s), ReprDeriveError::UnexpectedRepr));
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
        assert!(has_err(do_derive(s), ReprDeriveError::MultiplePrimitives));
    }

    #[test]
    fn test_struct_multiple_align() {
        let s = quote! {
            #[repr(C, align(4), align(16))]
            struct Foo;
        };
        assert!(has_err(do_derive(s), ReprDeriveError::MultipleAlign));
    }

    #[test]
    fn test_struct_multiple_packed() {
        let s = quote! {
            #[repr(C, packed, packed(2))]
            struct Foo;
        };
        assert!(has_err(do_derive(s), ReprDeriveError::MultiplePacked));
    }

    #[test]
    fn test_struct_invalid_packed_align() {
        let s = quote! {
            #[repr(C, packed("a"))]
            struct Foo;
        };
        assert!(has_err(do_derive(s), ReprDeriveError::InvalidPackedAlign));
    }

    #[test]
    fn test_struct_bare_packed() {
        let s = quote! {
            #[repr(C, packed)]
            struct Foo;
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), ReprDeriveError::PackedNotSupported));
    }

    #[test]
    fn test_struct_align() {
        let s = quote! {
            #[repr(C, align(4))]
            struct Foo;
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), ReprDeriveError::AlignNotSupported));
    }

    #[test]
    fn test_struct_no_repr_c() {
        let s = quote! {
            struct Foo;
        };
        // TODO should be implemented in the future
        assert!(has_err(do_derive(s), ReprDeriveError::StructNeedsReprC));
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
        assert!(has_err(do_derive(s), ReprDeriveError::NoReprForUnion));
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
        assert!(has_err(do_derive(s), ReprDeriveError::NoPrimForEnum));
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
        assert!(has_err(do_derive(s), ReprDeriveError::FieldlessEnumNeedsAllValues));
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
        assert!(has_err(do_derive(s), ReprDeriveError::FieldEnumCannotSetValues));
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
            ReprDeriveError::NonLifetimeGenericsNotSupported
        ));
    }
}
