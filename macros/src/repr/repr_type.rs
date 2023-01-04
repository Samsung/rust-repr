//! Tools for extracting repr(C) info from struct and enum declarations.

use super::ReprDeriveError;
use syn::spanned::Spanned;
use syn::{Attribute, DeriveInput, Lit, LitInt, Meta, MetaList, NestedMeta};

pub struct ReprInfo {
    span: proc_macro2::Span,
    pub is_c: bool,                   // True if there's "C" in repr.
    pub primitive: Option<syn::Path>, // The "u8" in e.g. repr(u8).
    pub align: Option<LitInt>,
    pub packed: Option<LitInt>,
}

impl ReprInfo {
    pub fn new(span: proc_macro2::Span) -> Self {
        Self {
            span,
            is_c: false,
            primitive: None,
            align: None,
            packed: None,
        }
    }

    pub fn set_primitive(&mut self, p: syn::Path) -> syn::Result<()> {
        if self.primitive.is_some() {
            return Err(syn::Error::new(
                p.span(),
                ReprDeriveError::MultiplePrimitives,
            ));
        }
        self.primitive = Some(p);
        Ok(())
    }

    pub fn set_align(&mut self, a: &LitInt) -> syn::Result<()> {
        if self.align.is_some() {
            return Err(syn::Error::new(a.span(), ReprDeriveError::MultipleAlign));
        }
        self.align = Some(a.clone());
        Ok(())
    }

    pub fn set_packed(&mut self, a: &LitInt) -> syn::Result<()> {
        if self.packed.is_some() {
            return Err(syn::Error::new(a.span(), ReprDeriveError::MultiplePacked));
        }
        self.packed = Some(a.clone());
        Ok(())
    }
    // TODO validate by enum / struct
    pub fn get_enum_primitive(&self) -> syn::Result<&syn::Path> {
        self.primitive
            .as_ref()
            .ok_or_else(|| syn::Error::new(self.span, ReprDeriveError::NoPrimForEnum))
    }
}

fn get_repr_attribute(a: &Attribute) -> Option<MetaList> {
    let a = match a.parse_meta() {
        Ok(a) => a,
        _ => return None,
    };
    let a = match a {
        Meta::List(l) => l,
        _ => return None,
    };
    if !a.path.is_ident("repr") {
        return None;
    }
    Some(a)
}

fn is_primitive(p: &syn::Path) -> bool {
    let primitives = [
        "u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32", "i64", "i128", "isize",
    ];
    primitives.iter().any(|i| p.is_ident(i))
}

fn extract_repr_info(l: &MetaList, info: &mut ReprInfo) -> syn::Result<()> {
    for item in &l.nested {
        match item {
            NestedMeta::Meta(Meta::Path(p)) => {
                if p.is_ident("C") {
                    info.is_c = true;
                } else if p.is_ident("packed") {
                    info.set_packed(&LitInt::new("1", item.span()))?;
                } else if is_primitive(p) {
                    info.set_primitive(p.clone())?;
                } else {
                    return Err(syn::Error::new(
                        item.span(),
                        ReprDeriveError::UnexpectedRepr,
                    ));
                }
            }
            NestedMeta::Meta(Meta::List(l)) => {
                if !l.path.is_ident("packed") && !l.path.is_ident("align") {
                    continue;
                }
                match l.nested.first() {
                    Some(NestedMeta::Lit(Lit::Int(i))) => {
                        if l.path.is_ident("packed") {
                            info.set_packed(i)?;
                        } else {
                            info.set_align(i)?;
                        }
                    }
                    _ => {
                        return Err(syn::Error::new(
                            item.span(),
                            ReprDeriveError::InvalidPackedAlign,
                        ))
                    }
                }
            }
            _ => {
                return Err(syn::Error::new(
                    item.span(),
                    ReprDeriveError::UnexpectedRepr,
                ))
            }
        }
    }
    Ok(())
}

pub fn get_repr_type(d: &DeriveInput) -> syn::Result<ReprInfo> {
    let mut info: ReprInfo = ReprInfo::new(d.span());
    for attr in d.attrs.iter().filter_map(get_repr_attribute) {
        extract_repr_info(&attr, &mut info)?;
    }
    Ok(info)
}

#[cfg(test)]
mod test {
    use super::*;
    use quote::quote;

    #[test]
    fn test_repr_info_align() {
        let formats = vec![
            quote! {
                #[repr(C, align(8))]
                struct Foobar;
            },
            quote! {
                #[repr(C)]
                #[repr(align(8))]
                struct Foobar;
            }
        ];

        for f in formats {
            let di: DeriveInput = syn::parse2(f).unwrap();
            let ri = get_repr_type(&di).unwrap();
            assert!(ri.align.unwrap().to_string() == "8")
        }
    }
}
