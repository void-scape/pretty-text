use pretty_text_parser::{Arg, CommandKind, Item, Span, Style};
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
    let spans = pretty_text_parser::parse(&text.value())
        .map(|items| items.0)
        .map_err(|e| syn::Error::new(text.span(), e))?
        .into_iter()
        .map(|span| tokenize_item(&span, &mut closures))
        .collect::<Result<Vec<_>, _>>()?;

    if closures.next().is_some() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected less callbacks",
        ));
    }

    Ok(quote! {
        ::bevy_pretty_text::parser::ParsedPrettyText::<bevy::prelude::Text>::new(vec![#(#spans,)*])
    })
}

pub fn parse_pretty_text2d(input: TokenStream) -> syn::Result<TokenStream2> {
    let PrettyTextInput { text, closures } = syn::parse(input)?;

    let mut closures = closures.into_iter().map(ToTokens::into_token_stream);
    let spans = pretty_text_parser::parse(&text.value())
        .map(|items| items.0)
        .map_err(|e| syn::Error::new(text.span(), e))?
        .into_iter()
        .map(|span| tokenize_item(&span, &mut closures))
        .collect::<Result<Vec<_>, _>>()?;

    if closures.next().is_some() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected less callbacks",
        ));
    }

    Ok(quote! {
        bevy_pretty_text::parser::ParsedPrettyText::<bevy::prelude::Text2d>::new(vec![#(#spans,)*])
    })
}

fn tokenize_item(
    item: &Item,
    handler: &mut impl Iterator<Item = TokenStream2>,
) -> syn::Result<TokenStream2> {
    Ok(match item {
        Item::Span { span, styles } => match span {
            Span::Text(text) => {
                let styles = styles.0.iter().map(tokenize_style);

                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: ::bevy_pretty_text::parser::Span::Text(std::borrow::Cow::Borrowed(#text)),
                        styles: ::bevy_pretty_text::style::Styles(vec![#(#styles,)*])
                    }
                }
            }
            Span::Items(items) => {
                let items = items
                    .iter()
                    .map(|span| tokenize_item(span, handler))
                    .collect::<Result<Vec<_>, _>>()?;
                let styles = styles.0.iter().map(tokenize_style);

                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: ::bevy_pretty_text::parser::Span::Bundles(vec![#(#items,)*]),
                        styles: ::bevy_pretty_text::style::Styles(vec![#(#styles,)*])
                    }
                }
            }
        },
        Item::Command { kind, value } => match kind {
            CommandKind::Pause => {
                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Command(
                        ::bevy_pretty_text::typewriter::hierarchy::TypewriterCommand::Pause(#value),
                    )
                }
            }
            CommandKind::Speed => {
                quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Command(
                        ::bevy_pretty_text::typewriter::hierarchy::TypewriterCommand::Speed(#value),
                    )
                }
            }
        },
        Item::Event(tag) => {
            quote! {
                ::bevy_pretty_text::parser::TextSpanBundle::Event(
                    ::bevy_pretty_text::typewriter::hierarchy::TypewriterEvent(#tag.into()),
                )
            }
        }
        Item::Callback => {
            let closure = handler.next().ok_or_else(|| {
                syn::Error::new(proc_macro2::Span::call_site(), "expected more callbacks")
            })?;

            let handler = quote! {
                ::bevy_pretty_text::typewriter::hierarchy::TypewriterCallback::new_with(
                    Box::new(|__world: &mut bevy::prelude::World| {
                        let closure = #closure;
                        let _ = __world.run_system_cached(closure);
                    })
                )
            };

            quote! { ::bevy_pretty_text::parser::TextSpanBundle::Callback(#handler) }
        }
    })
}

fn tokenize_style(style: &Style) -> proc_macro2::TokenStream {
    let tag = style.tag;
    let args = style.args.iter().map(|arg| match arg {
        Arg::Positioned(tag) => {
            quote::quote! {
                ::bevy_pretty_text::style::Arg::Positioned(#tag.into())
            }
        }
        Arg::Named { name, value } => {
            quote! {
                ::bevy_pretty_text::style::Arg::Named {
                    field: #name.into(),
                    value: #value.into(),
                }
            }
        }
    });

    quote! {
        ::bevy_pretty_text::style::Style {
            tag: #tag.into(),
            args: vec![#(#args),*]
        }
    }
}
