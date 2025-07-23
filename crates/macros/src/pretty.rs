use pretty_text::parser::{Span, TextSpanBundle};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};

struct PrettyTextInput {
    text: syn::LitStr,
    closures: Vec<syn::Expr>,
}

impl syn::parse::Parse for PrettyTextInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let text = input.parse()?;
        let mut closures = Vec::new();

        while !input.is_empty() {
            input.parse::<syn::Token![,]>()?;
            if input.is_empty() {
                break;
            }
            closures.push(input.parse()?);
        }

        Ok(PrettyTextInput { text, closures })
    }
}

pub fn parse_pretty_text(input: TokenStream) -> syn::Result<TokenStream2> {
    let PrettyTextInput { text, closures } = syn::parse(input)?;

    let mut closures = closures.into_iter().map(ToTokens::into_token_stream);
    let spans = pretty_text::parser::PrettyParser::spans(&text.value())
        .map_err(|e| syn::Error::new(text.span(), e))?
        .spans
        .into_iter()
        .map(|span| tokenize_span(&span, &mut closures))
        .collect::<Result<Vec<_>, _>>()?;

    if closures.next().is_some() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected less callbacks",
        ));
    }

    Ok(quote! {
        bevy_pretty_text::parser::PrettyTextSpans::<bevy::prelude::Text>::new(vec![#(#spans,)*])
    })
}

pub fn parse_pretty_text2d(input: TokenStream) -> syn::Result<TokenStream2> {
    let PrettyTextInput { text, closures } = syn::parse(input)?;

    let mut closures = closures.into_iter().map(ToTokens::into_token_stream);
    let spans = pretty_text::parser::PrettyParser2d::spans(&text.value())
        .map_err(|e| syn::Error::new(text.span(), e))?
        .spans
        .into_iter()
        .map(|span| tokenize_span(&span, &mut closures))
        .collect::<Result<Vec<_>, _>>()?;

    if closures.next().is_some() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected less callbacks",
        ));
    }

    Ok(quote! {
        bevy_pretty_text::parser::PrettyTextSpans::<bevy::prelude::Text2d>::new(vec![#(#spans,)*])
    })
}

fn tokenize_span(
    span: &TextSpanBundle,
    handler: &mut impl Iterator<Item = TokenStream2>,
) -> syn::Result<TokenStream2> {
    Ok(match span {
        TextSpanBundle::Span { span, mods } => match span {
            Span::Text(text) => {
                let text = text.as_ref();
                let mods = &mods.0;
                quote! {
                    bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: bevy_pretty_text::parser::Span::Text(std::borrow::Cow::Borrowed(#text)),
                        mods: bevy_pretty_text::parser::Modifiers(vec![#(#mods,)*])
                    }
                }
            }
            Span::Bundles(bundles) => {
                let bundles = bundles
                    .iter()
                    .map(|span| tokenize_span(span, handler))
                    .collect::<Result<Vec<_>, _>>()?;
                let mods = &mods.0;

                quote! {
                    bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: bevy_pretty_text::parser::Span::Bundles(vec![#(#bundles,)*]),
                        mods: bevy_pretty_text::parser::Modifiers(vec![#(#mods,)*])
                    }
                }
            }
        },
        TextSpanBundle::Effect(effect) => {
            quote! { bevy_pretty_text::parser::TextSpanBundle::Effect(#effect) }
        }
        TextSpanBundle::Event(tag) => {
            quote! { bevy_pretty_text::parser::TextSpanBundle::Event(#tag) }
        }
        TextSpanBundle::Callback(_) => {
            let closure = handler.next().ok_or_else(|| {
                syn::Error::new(proc_macro2::Span::call_site(), "expected more callbacks")
            })?;

            let handler = quote! {
                bevy_pretty_text::type_writer::hierarchy::TypeWriterCallback::new_with(
                    Box::new(|__world: &mut bevy::prelude::World| {
                        let closure = #closure;
                        let _ = __world.run_system_cached(closure);
                    })
                )
            };

            quote! { bevy_pretty_text::parser::TextSpanBundle::Callback(#handler) }
        }
    })
}
