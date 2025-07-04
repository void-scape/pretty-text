extern crate proc_macro;

use proc_macro::TokenStream;

mod material;
mod pretty;

#[proc_macro_derive(TextMaterial2d, attributes(atlas, pretty_text_path))]
pub fn derive_text_material2d(input: TokenStream) -> TokenStream {
    material::derive_text_material2d_inner(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn pretty(input: TokenStream) -> TokenStream {
    pretty::parse_pretty_text(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
