extern crate proc_macro;

use proc_macro::TokenStream;
use syn::parse_macro_input;

mod dynamic_effect;
mod material;
mod pretty;
mod syntax;

const ATTR_IDENT: &str = "pretty_text";

#[proc_macro_derive(GlyphMaterial, attributes(pretty_text))]
pub fn derive_glyph_material(input: TokenStream) -> TokenStream {
    material::derive_glyph_material_inner(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(DynamicEffect, attributes(pretty_text))]
pub fn derive_dynamic_effect(input: TokenStream) -> TokenStream {
    dynamic_effect::derive_dynamic_effect_inner(input)
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

#[proc_macro_attribute]
pub fn parser_syntax(attr: TokenStream, item: TokenStream) -> TokenStream {
    syntax::parser_syntax_inner(attr.into(), parse_macro_input!(item as syn::ItemStruct))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
