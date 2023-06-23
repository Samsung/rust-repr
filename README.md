A crate for safely converting arbitrary memory into Rust types.

Intro
=====

Imagine you have an enum type, like so:

```rust
#[repr(u8)]
enum Foo {
    FOO(u16, u32),
    BAR,
    BAZ(usize, *const ()),
}
```

Say that you have a pointer to that enum that came from an untrusted context,
like a wonky C API or from a userspace process to your kernel:

```rust
let foo: *const Foo = todo!()
```

While pointer alignment is easy to verify, "untrusted context" means that the
memory behind the pointer can be arbitrary. We can't simply convert the pointer
to such memory to a reference, since [this can lead to undefined
behaviour](https://doc.rust-lang.org/reference/behavior-considered-undefined.html).

Fortunately, the layout for properly defined `#[repr(C)]` types is
[well-defined](https://doc.rust-lang.org/reference/type-layout.html).
Unfortunately, working with this layout involves writing a lot of boilerplate,
especially for enums.

This crate does the boilerplate for you, like so:

```rust
use isrepr::{IsRepr, Repr, ReprError};
use core::convert::TryInto;
use core::mem::transmute;

#[derive(IsRepr, Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
enum Foo {
    FOO(u16, u32),
    BAR,
    BAZ(usize, *const ()),
}

// Repr<Foo> can have any memory contents.
fn bar(f_repr: Repr<Foo>) -> Result<Foo, ReprError> {
    f_repr.repr_try_into()
}

fn main() {
    // Pretend that we're some untrusted context.
    let foo = bar(unsafe { transmute(Foo::BAR) }).unwrap();
    assert_eq!(foo, Foo::BAR);

    // Send an invalid value!
    bar(unsafe { transmute(17u8) }).unwrap_err();
}
```

Links
=====

* [Docs.rs documentation](https://docs.rs/isrepr)
