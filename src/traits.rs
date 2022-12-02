//! Repr traits and generic types.

use core::marker::PhantomData;

use crate::static_assert::static_check_layout;

/// Error type used when conversion from underlying type fails.
#[derive(Clone, Copy, core::fmt::Debug, PartialEq, Eq)]
pub struct ReprError;

/// Trait for types that have canonical underlying representations.
///
/// This trait is implemented by various types like primitive types and pointers. It's also derived
/// by the [`IsRepr`](super::IsRepr) derive macro. In most cases you can rely on
/// [`IsRepr`](super::IsRepr) to derive this trait for you.
///
/// # Safety
///
/// The following should be satisfied in order for trait implementation to be sound:
/// * `Self` and `Self::Raw` should have the same memory layout, that is both size and alignment.
/// * `Self::Raw` should be safe to transmute into from arbitrary memory.
/// * Dropping a value should be a no-op, both for `Self` and for `Self::Raw`.
/// * If `Self::raw_is_valid(r: &Self::Raw)` returns Ok, then both casting `r` to `&Self` and
///   transmuting `*r` to `Self` should be sound.
pub unsafe trait HasRepr: Sized {
    /// The underlying type.
    ///
    /// Except for fieldless enums, this type is opaque.
    type Raw: Clone + Copy + core::fmt::Debug;

    /// Validate that the raw type can be safely transmuted to `Self`.
    fn raw_is_valid(value: &Self::Raw) -> Result<(), ReprError>;

    /// Convert directly from raw type.
    fn try_from_raw(value: Self::Raw) -> Result<Self, ReprError> {
        static_check_layout::<Self, Self::Raw>();
        Self::raw_is_valid(&value)?;
        Ok(unsafe { core::mem::transmute_copy(&value) })
    }

    /// Convert from a type-safe wrapper [`Repr`] over the raw type.
    fn try_from_repr(value: Repr<Self>) -> Result<Self, ReprError> {
        Self::try_from_raw(value.0)
    }

    /// Convert from reference to [`Repr`] into reference to self.
    fn try_from_ref(value: &Repr<Self>) -> Result<&Self, ReprError> {
        static_check_layout::<Self, Self::Raw>();
        Self::raw_is_valid(&value.0)?;
        Ok(unsafe { &*(value as *const Repr<Self> as *const Self) })
    }

    /// Transmute Self into a [`Repr`].
    ///
    /// In general, converting into `Repr` should be unnecessary, since only external-facing
    /// interfaces need to work with possibly-invalid values.
    fn into_repr(self) -> Repr<Self> {
        unsafe { core::mem::transmute_copy(&self) }
    }
}

/// Helper trait for converting representations into types. Useful for fieldless enums.
pub trait RawTryInto<T: HasRepr> {
    fn raw_try_into(self) -> Result<T, ReprError>;
}

impl<T: HasRepr> RawTryInto<T> for T::Raw {
    fn raw_try_into(self) -> Result<T, ReprError> {
        T::try_from_raw(self)
    }
}

/// Type-checked underlying representation of a type.
///
/// This type wraps the `Raw` type from [`HasRepr`]. This is also what you want to use as the
/// underlying representation of your type, as it's statically checked and won't be confused for
/// another type's representation.
///
/// For example, this does not compile:
/// ```compile_fail
/// use repr::{IsRepr, Repr};
/// use core::mem::transmute;
///
/// #[derive(IsRepr, Clone, Copy)]
/// #[repr(u8)]
/// enum Foo {
///     FOO = 1,
/// }
///
/// #[derive(IsRepr, Clone, Copy)]
/// #[repr(u8)]
/// enum Bar {
///     FOO = 1,
/// }
///
/// fn foo(f: Repr<Foo>) {}
///
/// let g: Repr<Bar> = unsafe { transmute(Bar::FOO) };
/// foo(g);
/// ```
// Doc for PhantomData say that it's better to use `*const T` so that we don't imply ownership.
#[repr(transparent)]
pub struct Repr<T: HasRepr>(T::Raw, PhantomData<*const T>);

impl<T: HasRepr> Repr<T> {
    /// Try to convert `Repr` to the `HasRepr` type.
    pub fn repr_try_into(self) -> Result<T, ReprError> {
        T::try_from_repr(self)
    }

    /// Try to convert reference to `Repr` to he `HasRepr` type.
    pub fn ref_try_into(&self) -> Result<&T, ReprError> {
        T::try_from_ref(self)
    }

    /// Convert an underlying type into `Repr`.
    ///
    /// Useful for converting fieldless enum values like `u8` into `Repr`.
    pub fn from_raw(v: T::Raw) -> Self {
        Self(v, Default::default())
    }
}

// Can't auto-derive Clone and Copy because of PhantomData.
impl<Enum: HasRepr> Clone for Repr<Enum> {
    fn clone(&self) -> Self {
        Self(self.0, Default::default())
    }
}
impl<Enum: HasRepr> Copy for Repr<Enum> {}

// Custom Debug that skips PhantomData.
impl<Enum: HasRepr> core::fmt::Debug for Repr<Enum> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Repr").field(&self.0).finish()
    }
}
