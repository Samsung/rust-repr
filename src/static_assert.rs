use core::marker::PhantomData;

// Some magic for comparing layouts at compile time.
const fn compare_layouts<T: Sized, S: Sized>() {
    if core::mem::size_of::<T>() != core::mem::size_of::<S>() {
        panic!("Type size mismatch");
    }
    if core::mem::align_of::<T>() != core::mem::align_of::<S>() {
        panic!("Type alignment mismatch");
    }
}

struct LayoutCheck<T: Sized, S: Sized>(PhantomData<T>, PhantomData<S>);

impl<T: Sized, S: Sized> LayoutCheck<T, S> {
    const CHECK: () = compare_layouts::<T, S>();
}

// This is for a static check, we know what we are doing
#[allow(clippy::let_unit_value)]
pub(super) fn static_check_layout<T: Sized, S: Sized>() {
    let _ = LayoutCheck::<T, S>::CHECK;
}
