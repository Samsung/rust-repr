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
/// * `#[derive(IsRepr)]` should be placed above all `#[repr(C)]`-like attributes, otherwise it's not
///   guaranteed to process them.
/// * Parametrized types with parameters that are not lifetimes are not supported.
/// * [`repr(transparent)`] is not supported. You can pretty easily derive [`IsRepr`] manually in
///   this situation.
///
pub use repr_macros::IsRepr;

pub use traits::{HasRepr, RawTryInto, Repr, ReprError};

// Needed for macros work in tests.
#[cfg(test)]
extern crate self as repr;

#[cfg(test)]
mod test {
    use crate::{
        traits::{HasRepr, Repr, ReprError},
        IsRepr,
    };
    use core::{
        marker::PhantomData,
        mem::{align_of, size_of, transmute_copy},
    };

    fn test_to_repr_and_back_is_id<T>(t: T)
    where
        T: Copy + HasRepr + Eq + core::fmt::Debug,
    {
        let t_repr: Repr<T> = unsafe { transmute_copy(&t) };
        let t2: T = t_repr.repr_try_into().unwrap();
        assert_eq!(t, t2);
    }

    fn to_repr<T: HasRepr>(t: &T) -> Repr<T> {
        unsafe { transmute_copy(t) }
    }

    unsafe fn test_equal<Struct, Member: Eq + core::fmt::Debug>(
        s: &Struct,
        m: &Member,
        offset: usize,
        unaligned: bool,
    ) {
        let raw_ptr = s as *const Struct as *const u8;
        let member_ptr = raw_ptr.offset(offset as isize) as *const Member;
        if !unaligned {
            assert_eq!(member_ptr.align_offset(align_of::<Member>()), 0);
        }

        let member = member_ptr.read_unaligned();
        assert_eq!(&member, m);
    }

    unsafe fn test_2_equal<Struct1, Struct2, Member: Eq + core::fmt::Debug>(
        s1: &Struct1,
        s2: &Struct2,
        m: &Member,
        offset: usize,
    ) {
        test_equal(s1, m, offset, false);
        test_equal(s2, m, offset, false);
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8)]
    enum Foobar {
        FOO = 5,
        BAR = 10,
    }

    #[test]
    fn test_value_enum_basic() {
        assert_eq!(Foobar::try_from_raw(8), Err(ReprError));
        assert_eq!(
            Foobar::try_from_repr(<Repr<Foobar>>::from_raw(5)),
            Ok(Foobar::FOO)
        );
    }

    #[test]
    fn test_value_enum_ref() {
        let mut base_val = <Repr<Foobar>>::from_raw(5);
        let foo = Foobar::try_from_ref(&base_val).unwrap();
        assert_eq!(*foo, Foobar::FOO);

        let foo = Foobar::try_from_mut(&mut base_val).unwrap();
        assert_eq!(*foo, Foobar::FOO);

        *foo = Foobar::BAR;

        assert_eq!(base_val.repr_try_into().unwrap(), Foobar::BAR);
    }

    #[test]
    fn test_value_enum_param() {
        fn inner(repr: Repr<Foobar>) -> Result<Foobar, ReprError> {
            repr.repr_try_into()
        }

        assert_eq!(inner(<Repr<Foobar>>::from_raw(10)), Ok(Foobar::BAR));
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8)]
    // 'pub' tests if we export repr type properly.
    pub enum Values {
        Unit,
        Tuple(u64, u64),
        Struct { x: u32, y: u32 },
    }

    #[test]
    fn test_value_enum_with_values() {
        for value in &[
            Values::Unit,
            Values::Tuple(5, 10),
            Values::Struct { x: 1, y: 2 },
        ] {
            test_to_repr_and_back_is_id(*value);
        }
    }

    #[test]
    fn test_invalid_enum_repr() {
        let value = Values::Tuple(5, 10);
        let mut value_repr: Repr<Values> = to_repr(&value);
        let repr_ptr = &mut value_repr as *mut Repr<Values> as *mut u8;
        unsafe { core::ptr::write(repr_ptr, 3) };
        Values::try_from_repr(value_repr).unwrap_err();
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8)]
    enum EnumC {
        Unit,
        Tuple(u64),
        Struct { x: u32, y: u32, z: u32 },
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8)]
    enum EnumNoC {
        Unit,
        Tuple(u64),
        Struct { x: u32, y: u32, z: u32 },
    }

    #[test]
    fn test_c_enum_repr_layout_matches() {
        assert_eq!(size_of::<EnumC>(), size_of::<Repr<EnumC>>());
        assert_eq!(align_of::<EnumC>(), align_of::<Repr<EnumC>>());

        let value = EnumC::Unit;
        let value_repr: Repr<EnumC> = to_repr(&value);
        unsafe {
            test_2_equal(&value, &value_repr, &0u8, 0);
        }

        let value = EnumC::Tuple(5);
        let value_repr: Repr<EnumC> = to_repr(&value);
        unsafe {
            let mut off = 0;
            test_2_equal(&value, &value_repr, &1u8, off);
            off += align_of::<u64>();
            test_2_equal(&value, &value_repr, &5u64, off);
        }

        let value = EnumC::Struct {
            x: 5,
            y: 0xdeadbeef,
            z: 66,
        };
        let value_repr: Repr<EnumC> = to_repr(&value);
        unsafe {
            let mut off = 0;
            test_2_equal(&value, &value_repr, &2u8, off);
            // For repr(C, u8), Union is aligned to max of member alignments, so it's u64 aligned.
            off += align_of::<u64>();
            test_2_equal(&value, &value_repr, &5u32, off);
            off += size_of::<u32>();
            test_2_equal(&value, &value_repr, &0xdeadbeefu32, off);
            off += size_of::<u32>();
            test_2_equal(&value, &value_repr, &66u32, off);
        }
    }

    #[test]
    fn test_no_c_enum_with_values() {
        for value in &[
            EnumNoC::Unit,
            EnumNoC::Tuple(5),
            EnumNoC::Struct { x: 1, y: 2, z: 3 },
        ] {
            test_to_repr_and_back_is_id(*value);
        }
    }

    #[test]
    fn test_no_c_enum_repr_layout_matches() {
        assert_eq!(size_of::<EnumNoC>(), size_of::<Repr<EnumNoC>>());
        assert_eq!(align_of::<EnumNoC>(), align_of::<Repr<EnumNoC>>());

        let value = EnumNoC::Unit;
        let value_repr: Repr<EnumNoC> = to_repr(&value);
        unsafe {
            test_2_equal(&value, &value_repr, &0u8, 0);
        }

        let value = EnumNoC::Tuple(5);
        let value_repr: Repr<EnumNoC> = to_repr(&value);
        unsafe {
            let mut off = 0;
            test_2_equal(&value, &value_repr, &1u8, off);
            off += align_of::<u64>();
            test_2_equal(&value, &value_repr, &5u64, off);
        }

        let value = EnumNoC::Struct {
            x: 5,
            y: 0xdeadbeef,
            z: 66,
        };
        let value_repr: Repr<EnumNoC> = to_repr(&value);
        unsafe {
            let mut off = 0;
            test_2_equal(&value, &value_repr, &2u8, off);
            // Tag is included in the union, so alignment is for u32.
            off += align_of::<u32>();
            test_2_equal(&value, &value_repr, &5u32, off);
            off += size_of::<u32>();
            test_2_equal(&value, &value_repr, &0xdeadbeefu32, off);
            off += size_of::<u32>();
            test_2_equal(&value, &value_repr, &66u32, off);
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct FooStruct {
        x: u8,
        y: u32,
        z: u64,
        t: u64,
    }

    #[test]
    fn test_struct_values() {
        test_to_repr_and_back_is_id(FooStruct {
            x: 5,
            y: 32,
            z: 67,
            t: 919,
        });
    }

    #[test]
    fn test_struct_repr_layout_matches() {
        let value = FooStruct {
            x: 5,
            y: 32,
            z: 67,
            t: 919,
        };
        let value_repr: Repr<FooStruct> = to_repr(&value);
        unsafe {
            let mut off = 0;
            test_2_equal(&value, &value_repr, &5u8, off);
            off += align_of::<u32>();
            test_2_equal(&value, &value_repr, &32u32, off);
            off += size_of::<u32>();
            test_2_equal(&value, &value_repr, &67u64, off);
            off += size_of::<u64>();
            test_2_equal(&value, &value_repr, &919u32, off);
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8)]
    enum EnumBoolAndArray {
        Bool(bool),
        Array([bool; 7]),
    }

    #[test]
    fn test_bool_enum_with_values() {
        for value in &[
            EnumBoolAndArray::Bool(true),
            EnumBoolAndArray::Array([true, false, false, false, true, true, true]),
        ] {
            test_to_repr_and_back_is_id(*value);
        }
    }

    #[test]
    fn test_bool_enum_invalid_values() {
        let e = EnumBoolAndArray::Bool(true);
        let mut e_repr: Repr<EnumBoolAndArray> = to_repr(&e);

        let repr_ptr = &mut e_repr as *mut Repr<EnumBoolAndArray> as *mut u8;
        let member_ptr = unsafe { repr_ptr.offset(1) };
        assert_eq!(unsafe { member_ptr.read() }, 1);
        unsafe { core::ptr::write(member_ptr, 3) };

        EnumBoolAndArray::try_from_repr(e_repr).unwrap_err();

        let e = EnumBoolAndArray::Array([false, false, false, true, false, false, false]);
        let mut e_repr: Repr<EnumBoolAndArray> = to_repr(&e);

        let repr_ptr = &mut e_repr as *mut Repr<EnumBoolAndArray> as *mut u8;
        let member_ptr = unsafe { repr_ptr.offset(4) }; // 4th array member
        assert_eq!(unsafe { member_ptr.read() }, 1);
        unsafe { core::ptr::write(member_ptr, 3) };

        EnumBoolAndArray::try_from_repr(e_repr).unwrap_err();
    }

    #[test]
    fn test_bool_enum_repr_layout_matches() {
        assert_eq!(
            size_of::<EnumBoolAndArray>(),
            size_of::<Repr<EnumBoolAndArray>>()
        );
        assert_eq!(
            align_of::<EnumBoolAndArray>(),
            align_of::<Repr<EnumBoolAndArray>>()
        );

        let value = EnumBoolAndArray::Bool(true);
        let value_repr: Repr<EnumBoolAndArray> = to_repr(&value);
        unsafe {
            test_2_equal(&value, &value_repr, &0u8, 0);
            test_2_equal(&value, &value_repr, &1u8, 1);
        }

        let value = EnumBoolAndArray::Array([true, false, false, false, true, true, true]);
        let value_repr: Repr<EnumBoolAndArray> = to_repr(&value);
        unsafe {
            test_2_equal(&value, &value_repr, &1u8, 0);
            test_2_equal(&value, &value_repr, &[1u8, 0u8, 0u8, 0u8, 1u8, 1u8, 1u8], 1);
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8)]
    enum Inner {
        Unit(u8),
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct Outer {
        x: u8,
        inner: Inner,
    }

    #[test]
    fn test_nested_repr() {
        test_to_repr_and_back_is_id(Outer {
            x: 5,
            inner: Inner::Unit(3),
        });
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct WithLifetime<'a> {
        x: u8,
        p: PhantomData<&'a u8>,
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8)]
    #[allow(dead_code)]
    enum NestedLifetime<'a> {
        One(*const u8, PhantomData<&'a u8>),
        Two(WithLifetime<'a>),
    }

    #[test]
    fn test_repr_with_lifetime_layout_matches() {
        assert_eq!(
            size_of::<WithLifetime<'static>>(),
            size_of::<Repr<WithLifetime<'static>>>()
        );
        assert_eq!(
            align_of::<WithLifetime<'static>>(),
            align_of::<Repr<WithLifetime<'static>>>()
        );
    }

    #[derive(IsRepr)]
    #[repr(C)]
    struct InnerLifetime<'a>(PhantomData<&'a u8>);

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct OuterLifetime<'a>(*const Repr<InnerLifetime<'a>>);

    #[test]
    fn test_nested_lifetime_struct() {
        // Basically test if it's properly generated and nothing more.
        test_to_repr_and_back_is_id(OuterLifetime(core::ptr::null()));
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, align(16))]
    struct AlignedU32 {
        x: u32,
        y: u32,
        z: u32,
        t: u32,
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, align(16))]
    struct HasAlignedU32 {
        foo: u32,
        bar: AlignedU32,
    }

    #[test]
    fn test_aligned_struct() {
        let a = HasAlignedU32 {
            foo: 1,
            bar: AlignedU32 {
                x: 2,
                y: 3,
                z: 4,
                t: 5,
            },
        };
        test_to_repr_and_back_is_id(a);
    }

    // Aligned enum behaves as if it was wrapped in a newtype struct with the specified alignment.
    // That is, its contents are unaffected by align, only its alignment as a whole.

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8, align(4))]
    #[allow(dead_code)]
    enum AlignedEnum {
        Bar,
        Foo(u8),
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct HasAlignedEnum {
        foo: u8,
        bar: AlignedEnum,
    }

    #[test]
    fn test_aligned_enum() {
        let a = HasAlignedEnum {
            foo: 1,
            bar: AlignedEnum::Foo(2),
        };
        test_to_repr_and_back_is_id(a);
        let a_repr = to_repr(&a);

        unsafe {
            test_2_equal(&a, &a_repr, &1u8, 4); // Aligned enum start, tag
            test_2_equal(&a, &a_repr, &2u8, 5); // Aligned enum data
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, u8, align(4))]
    #[allow(dead_code)]
    enum AlignedCEnum {
        Bar(u16),
        Foo(u8),
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C)]
    struct HasAlignedCEnum {
        foo: u8,
        bar: AlignedCEnum,
    }

    #[test]
    fn test_aligned_c_enum() {
        let a = HasAlignedCEnum {
            foo: 1,
            bar: AlignedCEnum::Foo(2),
        };
        test_to_repr_and_back_is_id(a);
        let a_repr = to_repr(&a);

        unsafe {
            test_2_equal(&a, &a_repr, &1u8, 4); // Aligned enum start, tag
            test_2_equal(&a, &a_repr, &2u8, 6); // Inner union start, alignment 2
        }
    }

    // Packed enums are disallowed in Rust.

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, packed)]
    struct Packed {
        foo: u8,
        bar: u32,
        baz: u64,
    }

    #[test]
    fn test_packed() {
        let a = Packed {
            foo: 1,
            bar: 2,
            baz: 3,
        };
        test_to_repr_and_back_is_id(a);
        let a_repr = to_repr(&a);

        unsafe {
            test_equal(&a, &1u8, 0, true);
            test_equal(&a_repr, &1u8, 0, true);
            test_equal(&a, &2u32, 1, true);
            test_equal(&a_repr, &2u32, 1, true);
            test_equal(&a, &3u64, 5, true);
            test_equal(&a_repr, &3u64, 5, true);
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(C, packed(2))]
    struct Packed2 {
        foo: u8,
        bar: u32,
        baz: u64,
    }

    #[test]
    fn test_packed2() {
        let a = Packed2 {
            foo: 1,
            bar: 2,
            baz: 3,
        };
        test_to_repr_and_back_is_id(a);
        let a_repr = to_repr(&a);

        unsafe {
            test_equal(&a, &1u8, 0, true);
            test_equal(&a_repr, &1u8, 0, true);
            test_equal(&a, &2u32, 2, true);
            test_equal(&a_repr, &2u32, 2, true);
            test_equal(&a, &3u64, 6, true);
            test_equal(&a_repr, &3u64, 6, true);
        }
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(transparent)]
    enum TransparentEnum {
        FOO(u32, (), ()),
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(transparent)]
    enum EmptyTransparentEnum {
        FOO,
    }

    #[test]
    fn test_transparent_enum() {
        let a = TransparentEnum::FOO(4, (), ());
        test_to_repr_and_back_is_id(a);
        let a_repr = to_repr(&a);
        unsafe {
            test_2_equal(&a, &a_repr, &4u32, 0); // Aligned enum start, tag
        }

        let a = EmptyTransparentEnum::FOO;
        assert_eq!(size_of::<EmptyTransparentEnum>(), 0);
        assert_eq!(align_of::<EmptyTransparentEnum>(), 1);
        test_to_repr_and_back_is_id(a);
    }

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(transparent)]
    struct TransparentStruct(u32, (), ());

    #[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(transparent)]
    struct EmptyTransparentStruct;

    #[test]
    fn test_transparent_struct() {
        let a = TransparentStruct(4, (), ());
        test_to_repr_and_back_is_id(a);
        let a_repr = to_repr(&a);
        unsafe {
            test_2_equal(&a, &a_repr, &4u32, 0); // Aligned enum start, tag
        }

        let a = EmptyTransparentStruct;
        assert_eq!(size_of::<EmptyTransparentStruct>(), 0);
        assert_eq!(align_of::<EmptyTransparentStruct>(), 1);
        test_to_repr_and_back_is_id(a);
    }
}
