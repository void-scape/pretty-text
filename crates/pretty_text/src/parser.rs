use bevy::ecs::relationship::RelatedSpawner;
use bevy::ecs::spawn::{SpawnRelatedBundle, SpawnWith};
use bevy::prelude::*;
use winnow::ascii::{multispace0, multispace1};
use winnow::combinator::{alt, delimited, eof, fail, opt, preceded, repeat, separated, terminated};
use winnow::error::{ContextError, ErrMode, ParserError, StrContext};
use winnow::stream::Stream;
use winnow::token::take_while;
use winnow::{Parser, prelude::*};

use crate::PrettyText;
use crate::style::SpanStyle;
use crate::type_writer::{TypeWriterEffect, TypeWriterEvent};

pub struct PrettyTextParser;

impl PrettyTextParser {
    pub fn parse(pretty_text: &str) -> Result<PrettyTextBundle<impl SpanSpawner>, String> {
        Self::parse_bundles(pretty_text).map(|spans| {
            (
                PrettyText,
                Text2d::default(),
                Children::spawn(SpawnWith(spawn_spans(spans))),
            )
        })
    }

    pub fn parse_bundles(pretty_text: &str) -> Result<Vec<TextSpanBundle>, String> {
        tokenize
            .parse(pretty_text)
            .map_err(|err| err.to_string())
            .and_then(|tokens| {
                parse_tokens.parse(&tokens).map_err(|err| {
                    pretty_print_token_err(pretty_text, err.input(), err.offset(), err.inner())
                })
            })
    }
}

pub trait SpanSpawner: FnOnce(&mut RelatedSpawner<ChildOf>) + Send + Sync + 'static {}
impl<F> SpanSpawner for F where F: FnOnce(&mut RelatedSpawner<ChildOf>) + Send + Sync + 'static {}

pub type PrettyTextBundle<F> = (
    PrettyText,
    Text2d,
    SpawnRelatedBundle<ChildOf, SpawnWith<F>>,
);

pub enum TextSpanBundle {
    Span {
        span: TextSpan,
        style: SpanStyle,
        effects: Vec<PrettyTextEffect>,
    },
    Effect(TypeWriterEffect),
    Event(TypeWriterEvent),
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for TextSpanBundle {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Span {
                span,
                style,
                effects,
            } => {
                let text = &span.0;
                quote::quote! {
                    ::bevy_pretty_text::parser::TextSpanBundle::Span {
                        span: ::bevy::text::TextSpan(#text.into()),
                        style: #style,
                        effects: vec![#(#effects,)*]
                    }
                }
            }
            Self::Effect(effect) => {
                quote::quote! { ::bevy_pretty_text::parser::TextSpanBundle::Effect(#effect) }
            }
            Self::Event(event) => {
                quote::quote! { ::bevy_pretty_text::parser::TextSpanBundle::Event(#event) }
            }
        });
    }
}

#[derive(Component)]
pub(crate) struct PrettyTextEffectCollection(pub Vec<PrettyTextEffect>);

pub struct PrettyTextEffect {
    pub tag: String,
    pub args: Vec<String>,
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for PrettyTextEffect {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;
        let tag = &self.tag;
        let args = &self.args;
        tokens.append_all(quote::quote! {
            ::bevy_pretty_text::parser::PrettyTextEffect {
                tag: #tag.into(),
                args: vec![#(#args.into(),)*]
            }
        });
    }
}

pub fn spawn_spans(
    spans: impl IntoIterator<Item = TextSpanBundle> + Send + Sync + 'static,
) -> impl FnOnce(&mut RelatedSpawner<ChildOf>) + Send + Sync + 'static {
    |spawner| {
        for span in spans.into_iter() {
            match span {
                TextSpanBundle::Span {
                    span,
                    style,
                    effects,
                } => {
                    spawner.spawn((PrettyText, span, style, PrettyTextEffectCollection(effects)));
                }
                TextSpanBundle::Effect(effect) => {
                    spawner.spawn(effect);
                }
                TextSpanBundle::Event(event) => {
                    spawner.spawn(event);
                }
            }
        }
    }
}

fn parse_tokens(input: &mut &[Token]) -> ModalResult<Vec<TextSpanBundle>> {
    repeat::<_, _, Vec<_>, _, _>(1.., text_components).parse_next(input)
}

fn text_components(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    alt((
        speed,
        pause,
        normal_text,
        styled_effect_text,
        styled_text,
        event,
    ))
    .parse_next(input)
}

fn speed(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    delimited(
        Token::OpenAngle,
        token_str
            .verify_map(|value| value.parse::<f32>().ok())
            .map_err(ErrMode::cut),
        Token::CloseAngle,
    )
    .map(|speed| TextSpanBundle::Effect(TypeWriterEffect::Speed(speed)))
    .parse_next(input)
}

fn pause(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    delimited(
        Token::OpenBracket,
        token_str
            .verify_map(|value| value.parse::<f32>().ok())
            .map_err(ErrMode::cut),
        Token::CloseBracket,
    )
    .map(|dur| TextSpanBundle::Effect(TypeWriterEffect::Pause(dur)))
    .parse_next(input)
}

fn normal_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    token_str
        .map(|str| TextSpanBundle::Span {
            span: TextSpan::from(str),
            style: SpanStyle::Inherit,
            effects: Vec::new(),
        })
        .parse_next(input)
}

fn styled_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    (
        preceded(Token::BackTick, token_str),
        delimited(Token::Bar, token_str, Token::BackTick),
    )
        .map(|(str, style)| TextSpanBundle::Span {
            span: TextSpan::from(str),
            style: SpanStyle::Tag(String::from(style)),
            effects: Vec::new(),
        })
        .parse_next(input)
}

fn styled_effect_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    (
        preceded(Token::BackTick, token_str),
        opt(preceded(Token::Bar, token_str)),
        Token::BackTick,
        delimited(Token::OpenBracket, effects, Token::CloseBracket),
    )
        .map(|(str, style, _, effects)| TextSpanBundle::Span {
            span: TextSpan::from(str),
            style: style
                .map(|style| SpanStyle::Tag(String::from(style)))
                .unwrap_or(SpanStyle::Inherit),
            effects,
        })
        .parse_next(input)
}

fn effects(input: &mut &[Token]) -> ModalResult<Vec<PrettyTextEffect>> {
    match input.next_token() {
        Some(Token::Text(str)) => terminated(
            separated(
                1..,
                (
                    take_while(1.., |c| c != ' ' && c != '(').map(String::from),
                    opt(delimited(
                        ('(', multispace0),
                        separated(
                            0..,
                            delimited(
                                multispace0,
                                take_while(1.., |c: char| {
                                    c != ',' && c != ')' && !c.is_whitespace()
                                }),
                                multispace0,
                            )
                            .map(|s: &str| s.to_string()),
                            ',',
                        ),
                        (multispace0, ')'),
                    ))
                    .map(|args| args.unwrap_or_default())
                    .context(StrContext::Label("effect")),
                )
                    .map(|(tag, args)| PrettyTextEffect { tag, args }),
                multispace1,
            ),
            eof.map_err(ErrMode::cut)
                .context(StrContext::Label("effects")),
        )
        .parse_next(&mut &*str),
        _ => Err(ParserError::from_input(input)),
    }
}

fn event(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    delimited(Token::OpenCurly, token_str, Token::CloseCurly)
        .map(|tag| TextSpanBundle::Event(TypeWriterEvent(tag.into())))
        .parse_next(input)
}

fn token_str<'a>(input: &mut &'a [Token]) -> ModalResult<&'a str> {
    match input.next_token() {
        Some(Token::Text(str)) => Ok(str),
        _ => fail.parse_next(input),
    }
}

impl<'a> Parser<&[Token<'a>], Token<'a>, ErrMode<ContextError>> for Token<'a> {
    fn parse_next(
        &mut self,
        input: &mut &[Token<'a>],
    ) -> winnow::Result<Token<'a>, ErrMode<ContextError>> {
        (input.next_token() == Some(*self))
            .then_some(*self)
            .ok_or_else(|| ParserError::from_input(input))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token<'a> {
    Text(&'a str),
    BackTick,
    Bar,
    OpenBracket,
    CloseBracket,
    OpenAngle,
    CloseAngle,
    OpenCurly,
    CloseCurly,
}

impl Token<'_> {
    fn as_str(&self) -> &str {
        match self {
            Self::Text(str) => str,
            Self::BackTick => "`",
            Self::Bar => "|",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::OpenAngle => "<",
            Self::CloseAngle => ">",
            Self::OpenCurly => "{",
            Self::CloseCurly => "}",
        }
    }
}

fn tokenize<'a>(input: &mut &'a str) -> ModalResult<Vec<Token<'a>>> {
    repeat(0.., token).parse_next(input)
}

fn token<'a>(input: &mut &'a str) -> ModalResult<Token<'a>> {
    let special_tokens = ['`', '|', '[', ']', '<', '>', '{', '}'];

    alt((
        "`".map(|_| Token::BackTick),
        "|".map(|_| Token::Bar),
        "[".map(|_| Token::OpenBracket),
        "]".map(|_| Token::CloseBracket),
        "<".map(|_| Token::OpenAngle),
        ">".map(|_| Token::CloseAngle),
        "{".map(|_| Token::OpenCurly),
        "}".map(|_| Token::CloseCurly),
        take_while(1.., |c| !special_tokens.contains(&c)).map(Token::Text),
    ))
    .parse_next(input)
}

fn pretty_print_token_err(
    str_input: &str,
    input: &[Token],
    offset: usize,
    ctx: &ContextError,
) -> String {
    let mut arrow_str = String::new();
    if offset >= input.len() {
        for _ in 0..str_input.len() {
            arrow_str.push(' ');
        }
        arrow_str.push('^');
    } else {
        let location = input[offset];
        let offset = input
            .iter()
            .map(|token| token.as_str().len())
            .take(offset)
            .sum();

        for _ in 0..offset {
            arrow_str.push(' ');
        }
        for _ in offset..offset + location.as_str().len() {
            arrow_str.push('^');
        }
    }

    format!("failed to parse input\n{str_input}\n{arrow_str}\n{ctx}")
}
