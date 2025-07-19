extern crate proc_macro;

use proc_macro::TokenStream;

mod effect;
mod material;
mod pretty;

#[proc_macro_derive(TextMaterial2d, attributes(pretty_text))]
pub fn derive_text_material2d(input: TokenStream) -> TokenStream {
    material::derive_text_material2d_inner(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(DynamicEffect, attributes(pretty_text))]
pub fn derive_text_effect(input: TokenStream) -> TokenStream {
    effect::derive_text_effect_inner(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn pretty(input: TokenStream) -> TokenStream {
    pretty::parse_pretty_text(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
