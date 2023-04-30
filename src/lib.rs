//! Type conversions from arbitrary memory.
//!
//! ## Intro
//!
//! Rust allows us to specify stable layouts of types with `#[repr(C)]` and friends. However, this
//! does not allow us to cast any properly sized and aligned memory to those types, since some
//! values may be invalid and making such a cast in Rust is immediate undefined behaviour. For APIs
//! facing outside Rust, this is very inconvenient.
//!
//! This crate provides a [`HasRepr`] trait that defines the `Raw` associated type. That type has
//! the same layout as `Self`, but is valid for all memory contents. It also provides a method for
//! validating that the representation can be cast to a valid value, and methods that convert by
//! value and by reference that use this validating method.
//!
//! A derive macro [`IsRepr`] can derive an implementation of [`HasRepr`]. This derivation works
//! for most `repr(C)` types, as long as all their members implement [`HasRepr`] themselves.
//! Implementations of `HasRepr` are provided for most primitive types.
//!
//! As an extra, the module defines a simple `Repr<T>` wrapper which transparently wraps the
//! underlying representation of `T` and is guaranteed to be a unique type for every `T`.
//!
//! For example, a representation can be derived and used as follows:
//! ```
//! use repr::{IsRepr, Repr, ReprError};
//! use core::convert::TryInto;
//! use core::mem::transmute;
//!
//! #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
//! #[repr(u8)]
//! enum Foo {
//!     FOO = 1,
//!     BAR = 3,
//! }
//!
//! // We accept data from some "untrusted" context.
//! fn bar(f_repr: Repr<Foo>) -> Result<Foo, ReprError> {
//!     f_repr.repr_try_into()
//! }
//!
//! fn main() {
//!     // Pretend that we're some untrusted context.
//!     let foo = bar(unsafe { transmute(Foo::FOO) }).unwrap();
//!     assert_eq!(foo, Foo::FOO);
//!
//!     // Send an invalid value!
//!     bar(unsafe { transmute(17u8) }).unwrap_err();
//! }
//! ```
//!
//! ## Sources
//!
//! You can find Rust's type layout guarantees here:
//!
//! <https://doc.rust-lang.org/reference/type-layout.html>

#![no_std]

pub(crate) mod prims;
pub(crate) mod static_assert;
pub(crate) mod traits;

/// Automatic derivation of [`HasRepr`].
///
/// [`HasRepr`] can be derived for fieldless enums if all values are explicitly specified:
/// ```
/// use repr::IsRepr;
///
/// #[derive(IsRepr, Clone, Copy, Debug)]
/// #[repr(u8)]
/// enum Foo {
///     FOO = 1,
///     BAR = 3,
/// }
/// ```
///
/// Fieldless enums can be converted directly from their underlying types:
/// ```
/// use repr::{HasRepr, IsRepr, Repr, RawTryInto};
/// use core::mem::transmute;
///
/// #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
/// #[repr(u8)]
/// enum Foo {
///     FOO = 1,
///     BAR = 3,
/// }
///
/// let foo = Foo::FOO;
/// let foo_u8 = foo as u8;
/// let foo_2: Foo = foo_u8.raw_try_into().unwrap();
/// assert_eq!(foo, foo_2);
/// ```
///
/// It can be derived for enums with fields and for structs, as long as all members implement
/// [`HasRepr`]:
///
/// ```
/// mod my_crate {
///     use repr::IsRepr;
///
///     #[derive(IsRepr, Clone, Copy, Debug)]
///     #[repr(u8)]
///     enum Foo {
///         FOO(u8),
///         BAR(*const usize),
///     }
///
///     #[derive(IsRepr, Clone, Copy, Debug)]
///     #[repr(C)]
///     struct Bar {
///         foo: u8,
///         bar: Foo,
///     }
/// }
/// ```
///
/// It supports both enum representations, `repr(C, prim)` and `repr(prim)`:
///
/// ```
/// mod my_crate {
///     use repr::IsRepr;
///
///     #[derive(IsRepr, Clone, Copy, Debug)]
///     #[repr(u8)]
///     enum Foo {
///         FOO(u8),
///         BAR(*const usize),
///     }
///
///     #[derive(IsRepr, Clone, Copy, Debug)]
///     #[repr(C, u8)]
///     enum Bar {
///         FOO(u8),
///         BAR(*const usize),
///     }
/// }
/// ```
///
/// [`HasRepr`] types can be converted by reference:
///
/// ```
/// use repr::{HasRepr, IsRepr, Repr};
/// use core::mem::transmute;
/// #[derive(IsRepr, Clone, Copy, Debug)]
/// #[repr(u8)]
/// enum Foo {
///     FOO = 1,
///     BAR = 3,
/// }
///
/// let foo: Repr<Foo> = unsafe { transmute(Foo::FOO) };
/// let _foo_ref: &Foo = foo.ref_try_into().unwrap();
/// ```
///
/// ## `IsRepr` for types with lifetimes
///
/// [`IsRepr`] has basic support for structs with lifetimes, enough to support types like nested
/// `PhantomData<&'a _>`. This support is implemented by erasing lifetime information in the
/// underlying type. Any `PhantomData` types are represented by unit `()`, and any data member `t:
/// T<'a, 'b ...>` is represented by `T<'static, 'static ...>::Raw`.
///
/// Lifetime support extends to the type-checked [`Repr`] type, so code that tries to circumvent
/// lifetimes by casting to representation and back will not compile:
///
/// ```compile_fail
/// use repr::{HasRepr, IsRepr, Repr};
/// use core::marker::PhantomData;
///
/// #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
/// #[repr(C)]
/// struct WithLifetime<'a> {
///     x: u8,
///     p: PhantomData<&'a u8>,
/// }
///
/// fn bad_cast<'a>(wl: WithLifetime<'a>) -> WithLifetime<'static> {
///     let el = wl.into_repr();
///     let bad_wl: WithLifetime<'static> = el.repr_try_into().unwrap();
///     bad_wl
/// }
/// ```
///
/// ## Limitations
///
/// * `#[derive(IsRepr)]` should be placed above all `#[repr(C)]`-like attributes. There's no
///   specification on the order of processing attibute macros, but apparrently it is done top
///   down.
/// * Automatic derivation for parametrized types with parameters that are not lifetimes is not
///   supported.
///
pub use repr_macros::IsRepr;

pub use traits::{HasRepr, RawTryInto, Repr, ReprError};

// Needed for macros work in tests.
#[cfg(test)]
extern crate self as repr;

#[cfg(test)]
mod test {
    extern crate alloc;
    extern crate std;
    use alloc::format;

    use crate::{
        traits::{HasRepr, Repr, ReprError},
        IsRepr,
    };
    use std::{
        marker::PhantomData,
        mem::{align_of, size_of, transmute_copy},
    };

    fn to_repr<T: HasRepr>(t: &T) -> Repr<T> {
        unsafe { transmute_copy(t) }
    }

    unsafe fn raw_write<T, M>(t: &mut T, m: M, off: usize) {
        let tp = t as *mut _ as *mut u8;
        let tm = &m as *const _ as *const u8;
        let tp = tp.offset(off as isize);
        tp.copy_from(tm, size_of::<M>());
    }

    mod layout {
        extern crate alloc;
        extern crate std;
        use super::{raw_write, to_repr};
        use alloc::vec::Vec;

        use crate::{
            traits::{HasRepr, Repr, ReprError},
            IsRepr,
        };
        use core::panic::RefUnwindSafe;
        use std::sync::Mutex;
        use std::{
            collections::HashSet,
            mem::{align_of, size_of},
        };
        // Start by testing layout. Here's how we do it.
        //
        // In a type, we encode its expected layout, like so:
        // struct Foo {
        //     x: L<0, 0>,
        //     y: L<8, 1>,
        // }
        //
        // The L<N, ID> types have a custom implementation of HasRepr that verifies that they have
        // the right offset relative to the type being checked. In order to check that offset, we
        // set each L to a unique bit pattern, which then gets verified inside the type. FIXME:
        // This is not 100% perfect as it doesn't guarantee that padding doesn't have the same
        // value.

        // The type comes first. Dirty shortcut: T should be valid for any memory contents.
        trait Num: Sized + Copy + Eq + std::fmt::Debug + Default {}
        impl<T: Sized + Copy + Eq + std::fmt::Debug + Default> Num for T {}

        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        #[repr(C)]
        struct L<T: Num, const OFF: usize, const ID: u8>(T);

        impl<T: Num, const OFF: usize, const ID: u8> L<T, OFF, ID> {
            fn unique_byte_pattern() -> Vec<u8> {
                alloc::vec![0xffu8 - ID; size_of::<Self>()]
            }

            fn new() -> Self {
                let mut me = Self(Default::default());
                let mep = &mut me as *mut _ as *mut u8;
                let unique_byte_pattern = Self::unique_byte_pattern();
                unsafe {
                    mep.copy_from(unique_byte_pattern.as_ptr(), size_of::<Self>());
                }
                me
            }
        }

        unsafe impl<T: Num, const OFF: usize, const ID: u8> HasRepr for L<T, OFF, ID> {
            type Raw = Self;

            fn raw_is_valid(value: &Self::Raw) -> Result<(), ReprError> {
                let unique_byte_pattern = Self::unique_byte_pattern();
                let current_byte_pattern = unsafe {
                    std::slice::from_raw_parts(value as *const _ as *const u8, size_of::<Self>())
                };
                assert_eq!(&unique_byte_pattern, current_byte_pattern);
                add_verified_offset(ID);
                Ok(())
            }
        }

        // Now, something to verify we checked all members and exactly the members we want. Can't
        // pass arguments to IsRepr, so we access a global in HasRepr implementation.
        struct GlobalLayoutInfo {
            verified_offsets: Vec<u8>,
        }
        unsafe impl Send for GlobalLayoutInfo {}

        // Hack: use a separate mutex, since HasRepr implementation is called from a different
        // context.
        static OFFSET_CHECK_LOCK: Mutex<()> = Mutex::new(());
        static LAYOUT_INFO: Mutex<GlobalLayoutInfo> = Mutex::new(GlobalLayoutInfo {
            verified_offsets: Vec::new(),
        });

        fn add_verified_offset(off: u8) {
            LAYOUT_INFO.lock().unwrap().verified_offsets.push(off);
        }

        fn clear_verified_offsets() {
            LAYOUT_INFO.lock().unwrap().verified_offsets.clear();
        }

        fn compare_verified_offsets(to: &[u8]) {
            let offs = LAYOUT_INFO.lock().unwrap().verified_offsets.clone();
            let mut expected_set: HashSet<u8> = HashSet::new();
            expected_set.extend(to);
            let mut actual_set = HashSet::new();
            actual_set.extend(offs);
            assert_eq!(expected_set, actual_set);
        }

        fn test_layout<T: HasRepr>(t: &T, expected_offsets: &[u8])
        where
            T::Raw: RefUnwindSafe,
        {
            assert_eq!(size_of::<T>(), size_of::<Repr<T>>());
            assert_eq!(align_of::<T>(), align_of::<Repr<T>>());
            let t_repr = to_repr(t);
            let t_repr_ref = &t_repr;

            let _lock = OFFSET_CHECK_LOCK.lock().unwrap();

            // Unlock the global lock on panic
            let p = std::panic::catch_unwind(|| {
                clear_verified_offsets();
                let _new_t = t_repr_ref.ref_try_into().unwrap();
                compare_verified_offsets(expected_offsets);
            });
            drop(_lock);
            match p {
                Ok(r) => r,
                Err(e) => std::panic::panic_any(e),
            }
        }

        // We cannot test tag offset directly, so we just check if invalid tag causes conversion to fail.
        unsafe fn test_enum_invalid_tag<E: HasRepr + std::fmt::Debug, M>(
            e: &mut E,
            m: M,
            off: usize,
        ) {
            let mut e = to_repr(e);
            raw_write(&mut e, m, off);
            e.repr_try_into().unwrap_err();
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        enum ValuelessEnum {
            FOO = 5,
            BAR = 10,
        }

        #[test]
        fn test_valueless_enum() {
            test_layout(&ValuelessEnum::FOO, &[]);
            test_layout(&ValuelessEnum::BAR, &[]);
            unsafe {
                test_enum_invalid_tag(&mut ValuelessEnum::FOO, 15u8, 0);
            }
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        enum EnumWithValues {
            Unit,
            Tuple(L<u64, 8, 0>, L<u64, 16, 1>),
            Struct {
                x: L<u32, 4, 2>,
                y: L<u32, 8, 3>,
                z: L<u32, 12, 4>,
            },
        }

        #[test]
        fn test_enum_with_values() {
            test_layout(&EnumWithValues::Unit, &[]);
            test_layout(&EnumWithValues::Tuple(L::new(), L::new()), &[0, 1]);
            test_layout(
                &EnumWithValues::Struct {
                    x: L::new(),
                    y: L::new(),
                    z: L::new(),
                },
                &[2, 3, 4],
            );
            unsafe {
                test_enum_invalid_tag(&mut EnumWithValues::Unit, 3u8, 0);
            }
        }

        // In a repr(C) enum, tag is a separate member outside the union. This means that Struct.x
        // has offset 8 rather than 4, since Tuple forces alignment to 8.
        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C, u8)]
        enum CEnumWithValues {
            Unit,
            Tuple(L<u64, 8, 0>, L<u64, 16, 1>),
            Struct {
                x: L<u32, 8, 2>,
                y: L<u32, 12, 3>,
                z: L<u32, 16, 4>,
            },
        }

        #[test]
        fn test_c_enum_with_values() {
            test_layout(&CEnumWithValues::Unit, &[]);
            test_layout(&CEnumWithValues::Tuple(L::new(), L::new()), &[0, 1]);
            test_layout(
                &CEnumWithValues::Struct {
                    x: L::new(),
                    y: L::new(),
                    z: L::new(),
                },
                &[2, 3, 4],
            );
            unsafe {
                test_enum_invalid_tag(&mut CEnumWithValues::Unit, 3u8, 0);
            }
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C)]
        struct Struct {
            x: L<u8, 0, 0>,
            y: L<u32, 4, 1>,
            z: L<u64, 8, 2>,
            t: L<u64, 16, 3>,
        }

        #[test]
        fn test_struct() {
            test_layout(
                &Struct {
                    x: L::new(),
                    y: L::new(),
                    z: L::new(),
                    t: L::new(),
                },
                &[0, 1, 2, 3],
            );
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C)]
        struct StructWithArray {
            x: L<[u8; 3], 0, 0>,
            y: L<[u32; 3], 4, 1>,
            z: L<[u8; 5], 16, 2>,
            t: L<u64, 24, 3>,
        }

        #[test]
        fn test_struct_with_array() {
            test_layout(
                &StructWithArray {
                    x: L::new(),
                    y: L::new(),
                    z: L::new(),
                    t: L::new(),
                },
                &[0, 1, 2, 3],
            );
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C)]
        struct NestedReprOuter {
            x: L<u8, 0, 0>,
            inner: NestedReprInner,
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        enum NestedReprInner {
            Unit(L<u8, 2, 1>),
        }

        #[test]
        fn test_nested_repr() {
            test_layout(
                &NestedReprOuter {
                    x: L::new(),
                    inner: NestedReprInner::Unit(L::new()),
                },
                &[0, 1],
            );
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C, align(16))]
        struct Aligned16Outer {
            foo: L<u32, 0, 0>,
            bar: Aligned16Inner,
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C, align(16))]
        struct Aligned16Inner {
            x: L<u32, 16, 1>,
            y: L<u32, 20, 2>,
            z: L<u32, 24, 3>,
        }

        #[test]
        fn test_aligned_struct() {
            test_layout(
                &Aligned16Outer {
                    foo: L::new(),
                    bar: Aligned16Inner {
                        x: L::new(),
                        y: L::new(),
                        z: L::new(),
                    },
                },
                &[0, 1, 2, 3],
            );
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C)]
        struct StructWithAligned4Enum {
            foo: L<u8, 0, 0>,
            bar: Aligned4Enum,
        }

        // Aligned enum behaves as if it was wrapped in a newtype struct with the specified alignment.
        // That is, its contents are unaffected by align, only its alignment as a whole.
        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8, align(4))]
        #[allow(dead_code)]
        enum Aligned4Enum {
            Bar,
            Foo(L<u8, 5, 1>),
        }

        #[test]
        fn test_aligned_enum() {
            test_layout(
                &StructWithAligned4Enum {
                    foo: L::new(),
                    bar: Aligned4Enum::Foo(L::new()),
                },
                &[0, 1],
            );
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C)]
        struct StructWithCAligned4Enum {
            foo: L<u8, 0, 0>,
            bar: CAligned4Enum,
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C, u8, align(4))]
        #[allow(dead_code)]
        enum CAligned4Enum {
            Bar(L<u16, 6, 1>),
            Foo(L<u8, 6, 2>),
        }

        #[test]
        fn test_c_aligned_enum() {
            test_layout(
                &StructWithCAligned4Enum {
                    foo: L::new(),
                    bar: CAligned4Enum::Foo(L::new()),
                },
                &[0, 2],
            );
            test_layout(
                &StructWithCAligned4Enum {
                    foo: L::new(),
                    bar: CAligned4Enum::Bar(L::new()),
                },
                &[0, 1],
            );
        }

        // Packed enums are disallowed in Rust.
        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C, packed)]
        struct Packed {
            foo: L<u8, 0, 0>,
            bar: L<u32, 1, 1>,
            baz: L<u64, 5, 2>,
        }

        // Packed(n) aligns all members to smaller of their natural alignment and 2^n.
        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C, packed(2))]
        struct Packed2 {
            foo: L<u8, 0, 0>,
            bar: L<u32, 2, 1>,
            baz: L<u64, 6, 2>,
            bax: L<u8, 14, 3>,
            bav: L<u8, 15, 4>,
        }

        #[test]
        fn test_c_packed_enum() {
            test_layout(
                &Packed {
                    foo: L::new(),
                    bar: L::new(),
                    baz: L::new(),
                },
                &[0, 1, 2],
            );
            test_layout(
                &Packed2 {
                    foo: L::new(),
                    bar: L::new(),
                    baz: L::new(),
                    bax: L::new(),
                    bav: L::new(),
                },
                &[0, 1, 2, 3, 4],
            );
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(transparent)]
        enum TransparentEnum {
            FOO((), L<u32, 0, 0>, ()),
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(transparent)]
        enum EmptyTransparentEnum {
            FOO,
        }

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(transparent)]
        struct TransparentStruct((), L<u32, 0, 0>, ());

        #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(transparent)]
        struct EmptyTransparentStruct;

        #[test]
        fn test_transparent() {
            test_layout(&TransparentEnum::FOO((), L::new(), ()), &[0]);
            test_layout(&EmptyTransparentEnum::FOO, &[]);
            test_layout(&TransparentStruct((), L::new(), ()), &[0]);
            test_layout(&EmptyTransparentStruct, &[]);
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8)]
    enum ValuelessEnum {
        FOO = 5,
        BAR = 10,
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8)]
    // 'pub' tests if we export repr type properly.
    pub enum CEnumWithValues {
        Unit,
        Tuple(u64, u64),
        Struct { x: u32, y: u32, z: u32 },
    }

    #[test]
    fn test_valueless_enum_from_raw() {
        assert_eq!(ValuelessEnum::try_from_raw(8), Err(ReprError));
        assert_eq!(
            ValuelessEnum::try_from_repr(<Repr<ValuelessEnum>>::from_raw(5)),
            Ok(ValuelessEnum::FOO)
        );
    }

    #[test]
    fn test_valueless_enum_ref() {
        let mut base_val = <Repr<ValuelessEnum>>::from_raw(5);
        let foo = ValuelessEnum::try_from_ref(&base_val).unwrap();
        assert_eq!(*foo, ValuelessEnum::FOO);

        let foo = ValuelessEnum::try_from_mut(&mut base_val).unwrap();
        assert_eq!(*foo, ValuelessEnum::FOO);

        *foo = ValuelessEnum::BAR;

        assert_eq!(base_val.repr_try_into().unwrap(), ValuelessEnum::BAR);
    }

    #[test]
    fn test_value_enum_with_values_debug() {
        let value = CEnumWithValues::Unit;
        let value_repr: Repr<CEnumWithValues> = to_repr(&value);
        assert_eq!(
            format!("{:?}", value_repr),
            "Repr(CEnumWithValuesRepr { _repr_tag: 0, f_Unit: Unit })"
        );

        let value = CEnumWithValues::Tuple(5, 10);
        let value_repr: Repr<CEnumWithValues> = to_repr(&value);
        assert_eq!(
            format!("{:?}", value_repr),
            "Repr(CEnumWithValuesRepr { _repr_tag: 1, f_Tuple: Tuple(5, 10) })"
        );

        let value = CEnumWithValues::Struct { x: 1, y: 2, z: 3 };
        let mut value_repr: Repr<CEnumWithValues> = to_repr(&value);
        assert_eq!(
            format!("{:?}", value_repr),
            "Repr(CEnumWithValuesRepr { _repr_tag: 2, f_Struct: Struct { x: 1, y: 2, z: 3 } })"
        );

        let repr_ptr = &mut value_repr as *mut Repr<CEnumWithValues> as *mut u8;
        unsafe { core::ptr::write(repr_ptr, 3) };
        assert_eq!(
            format!("{:?}", value_repr),
            "Repr(CEnumWithValuesRepr { _repr_tag: 3 })"
        );
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8)]
    enum EnumWithBoolAndArray {
        Bool(bool),
        Array([bool; 7]),
    }

    #[test]
    fn test_bool_enum_with_values() {
        for value in &[
            EnumWithBoolAndArray::Bool(true),
            EnumWithBoolAndArray::Array([true, false, false, false, true, true, true]),
        ] {
            let r = to_repr(value);
            r.repr_try_into().unwrap();
        }

        let mut value = EnumWithBoolAndArray::Bool(true);
        unsafe { raw_write(&mut value, 2u8, 1) };
        let r = to_repr(&value);
        r.repr_try_into().unwrap_err();

        let mut value = EnumWithBoolAndArray::Array([true; 7]);
        unsafe { raw_write(&mut value, 2u8, 5) };
        let r = to_repr(&value);
        r.repr_try_into().unwrap_err();
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct StructWithLifetime<'a> {
        x: u8,
        p: PhantomData<&'a u8>,
    }

    #[test]
    fn test_repr_with_lifetime_layout_matches() {
        assert_eq!(
            size_of::<StructWithLifetime<'static>>(),
            size_of::<Repr<StructWithLifetime<'static>>>()
        );
        assert_eq!(
            align_of::<StructWithLifetime<'static>>(),
            align_of::<Repr<StructWithLifetime<'static>>>()
        );
    }

    #[derive(IsRepr)]
    #[repr(C)]
    struct NestedLifetimeInner<'a>(PhantomData<&'a u8>);

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct NestedLifetimeOuter<'a>(*const Repr<NestedLifetimeInner<'a>>);

    #[test]
    fn test_nested_lifetime_struct() {
        // Test if it's properly generated and nothing more.
        let _ = NestedLifetimeOuter(std::ptr::null());
    }

    #[derive(IsRepr)]
    #[repr(C)]
    struct UnsizedPhantomData(PhantomData<[u8]>);

    #[derive(IsRepr)]
    #[repr(C)]
    struct UnsizedPtr(*mut [u8]);
}
