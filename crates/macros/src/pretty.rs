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
    let spans = pretty_text::parser::PrettyTextParser::parse_bundles(&text.value())
        .map_err(|e| syn::Error::new(text.span(), e))?
        .into_iter()
        .map(|span| tokenize_span(&span, &mut closures))
        .collect::<Result<Vec<_>, _>>()?;

    if closures.next().is_some() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected less closures",
        ));
    }

    Ok(quote! {
        ::bevy_pretty_text::bundle::StaticPrettyTextSpans::new(const { &[#(#spans,)*] })
    })
}

fn tokenize_span(
    span: &TextSpanBundle,
    handler: &mut impl Iterator<Item = TokenStream2>,
) -> syn::Result<TokenStream2> {
    Ok(match span {
        TextSpanBundle::Span {
            span,
            style,
            effects,
        } => match span {
            Span::Text(text) => {
                let text = text.as_ref();
                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: ::bevy_pretty_text::parser::Span::Text(std::borrow::Cow::Borrowed(#text)),
                        style: #style,
                        effects: std::borrow::Cow::Borrowed(&[#(#effects,)*])
                    }
                }
            }
            Span::Bundles(bundles) => {
                let bundles = bundles
                    .iter()
                    .map(|span| tokenize_span(span, handler))
                    .collect::<Result<Vec<_>, _>>()?;

                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: ::bevy_pretty_text::parser::Span::Bundles(std::borrow::Cow::Borrowed(&[#(#bundles,)*])),
                        style: #style,
                        effects: std::borrow::Cow::Borrowed(&[#(#effects,)*])
                    }
                }
            }
        },
        TextSpanBundle::Effect(effect) => {
            quote! { ::bevy_pretty_text::parser::TextSpanBundle::Effect(#effect) }
        }
        TextSpanBundle::Event { tag, .. } => {
            if tag.0.is_some() {
                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Event {
                        tag: #tag,
                        handler: None,
                    }
                }
            } else {
                let closure = handler.next().ok_or_else(|| {
                    syn::Error::new(proc_macro2::Span::call_site(), "expected more closures")
                })?;

                let handler = if tag.0.is_none() {
                    quote! {
                        Some(::bevy_pretty_text::type_writer::TypeWriterEventHandler(
                            &|__world: &mut bevy::prelude::World| {
                                let closure = #closure;
                                let _ = __world.run_system_cached(closure);
                            }
                        ))
                    }
                } else {
                    quote! { None }
                };

                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Event {
                        tag: #tag,
                        handler: #handler,
                    }
                }
            }
        }
    })
}
