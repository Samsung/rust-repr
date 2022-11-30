use proc_macro::TokenStream;

pub(crate) mod repr;

#[proc_macro_derive(IsRepr)]
pub fn derive(input: TokenStream) -> TokenStream {
    match repr::do_derive(input.into()) {
        Err(e) => e.to_compile_error().into(),
        Ok(o) => o.into(),
    }
}
