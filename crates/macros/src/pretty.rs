use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

pub fn parse_pretty_text(input: TokenStream) -> syn::Result<TokenStream2> {
    let str: syn::LitStr = syn::parse(input)?;

    let spans = pretty_text::parser::PrettyTextParser::parse_bundles(&str.value())
        .map_err(|e| syn::Error::new(str.span(), e))?;

    Ok(quote! {
        (
            ::bevy_pretty_text::PrettyText,
            ::bevy::text::Text2d::default(),
            ::bevy::ecs::hierarchy::Children::spawn(
                bevy::ecs::spawn::SpawnWith(
                    ::bevy_pretty_text::parser::spawn_spans([#(#spans,)*])
                )
            )
        )
    })
}
