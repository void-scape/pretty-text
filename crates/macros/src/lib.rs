extern crate proc_macro;

use proc_macro::TokenStream;

mod effect;
mod material;
mod pretty;

const ATTR_IDENT: &str = "pretty_text";

#[proc_macro_derive(GlyphMaterial, attributes(pretty_text))]
pub fn derive_glyph_material(input: TokenStream) -> TokenStream {
    material::derive_glyph_material_inner(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(DynamicGlyphEffect, attributes(pretty_text))]
pub fn derive_dynamic_effect(input: TokenStream) -> TokenStream {
    effect::derive_dynamic_effect_inner(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn pretty(input: TokenStream) -> TokenStream {
    pretty::parse_pretty_text(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn pretty2d(input: TokenStream) -> TokenStream {
    pretty::parse_pretty_text2d(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
