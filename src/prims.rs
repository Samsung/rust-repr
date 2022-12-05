use core::marker::PhantomData;

use crate::{HasRepr, ReprError};

// Implementations for basic types.
// Compiler complains when trying to use sealed traits for trivial implementations, so we macro our
// way out.
macro_rules! trivial {
    ($t: ty) => {
        unsafe impl HasRepr for $t {
            type Raw = $t;
            fn raw_is_valid(_: &$t) -> Result<(), ReprError> {
                Ok(())
            }
        }
    };
}

trivial!(u8);
trivial!(u16);
trivial!(u32);
trivial!(u64);
trivial!(u128);
trivial!(usize);
trivial!(i8);
trivial!(i16);
trivial!(i32);
trivial!(i64);
trivial!(i128);
trivial!(isize);
trivial!(f32);
trivial!(f64);

unsafe impl<T> HasRepr for *const T {
    type Raw = Self;
    fn raw_is_valid(_: &*const T) -> Result<(), ReprError> {
        Ok(())
    }
}

unsafe impl<T> HasRepr for *mut T {
    type Raw = Self;
    fn raw_is_valid(_: &*mut T) -> Result<(), ReprError> {
        Ok(())
    }
}

//PhantomData. Strip type info so we don't have to worry about lifetimes.
unsafe impl<T> HasRepr for PhantomData<T> {
    type Raw = ();
    fn raw_is_valid(_: &()) -> Result<(), ReprError> {
        Ok(())
    }
}

// bool
unsafe impl HasRepr for bool {
    type Raw = u8;
    fn raw_is_valid(value: &u8) -> Result<(), ReprError> {
        match value {
            0 => Ok(()),
            1 => Ok(()),
            _ => Err(ReprError),
        }
    }
}

// arrays
unsafe impl<T: HasRepr, const N: usize> HasRepr for [T; N] {
    type Raw = [<T as HasRepr>::Raw; N];

    fn raw_is_valid(value: &Self::Raw) -> Result<(), ReprError> {
        for v in value.iter() {
            T::raw_is_valid(v)?;
        }
        Ok(())
    }
}
