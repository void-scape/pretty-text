use std::borrow::Cow;

use bevy::prelude::*;
use winnow::ascii::{multispace0, multispace1};
use winnow::combinator::{alt, delimited, eof, fail, opt, preceded, repeat, separated, terminated};
use winnow::error::{ContextError, ErrMode, ParserError, StrContext};
use winnow::stream::Stream;
use winnow::token::take_while;
use winnow::{Parser, prelude::*};

use crate::PrettyText;
use crate::bundle::PrettyTextSpans;
use crate::style::SpanStyle;
use crate::type_writer::{TypeWriterEffect, TypeWriterEventHandler, TypeWriterEventTag};

pub struct PrettyTextParser;

impl PrettyTextParser {
    pub fn parse(pretty_text: &str) -> Result<PrettyTextSpans, String> {
        Self::parse_bundles(pretty_text).map(PrettyTextSpans::new)
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

#[derive(Debug, Clone)]
pub enum TextSpanBundle {
    Span {
        span: Span,
        style: SpanStyle,
        effects: Cow<'static, [PrettyTextEffect]>,
    },
    Effect(TypeWriterEffect),
    Event {
        tag: TypeWriterEventTag,
        handler: Option<TypeWriterEventHandler>,
    },
}

impl TextSpanBundle {
    pub fn with_parent(self, entity: &mut EntityWorldMut) {
        match self {
            TextSpanBundle::Span {
                span,
                style,
                effects,
            } => match span {
                Span::Text(text) => {
                    entity.with_child((
                        PrettyText,
                        TextSpan::new(text),
                        style,
                        PrettyTextEffectCollection(effects),
                    ));
                }
                Span::Bundles(bundles) => {
                    let mut effects = effects.to_vec();
                    for bundle in bundles.iter() {
                        bundle.with_parent_ref(entity, &style, &mut effects);
                    }
                }
            },
            TextSpanBundle::Effect(effect) => {
                entity.with_child(effect);
            }
            TextSpanBundle::Event { tag, handler } => match handler {
                Some(handler) => {
                    entity.with_child((tag, handler));
                }
                None => {
                    entity.with_child(tag);
                }
            },
        }
    }

    pub fn with_parent_ref(
        &self,
        entity: &mut EntityWorldMut,
        parent_style: &SpanStyle,
        parent_effects: &mut Vec<PrettyTextEffect>,
    ) {
        match self {
            TextSpanBundle::Span {
                span,
                style,
                effects,
            } => match span {
                Span::Text(text) => {
                    let mut new_effects = effects.to_vec();
                    if !parent_effects.is_empty() {
                        for effect in parent_effects.iter() {
                            new_effects.push(effect.clone());
                        }
                    }

                    let mut new_style = style.clone();
                    if new_style == SpanStyle::Inherit {
                        new_style = parent_style.clone();
                    }

                    entity.with_child((
                        PrettyText,
                        TextSpan::new(text.as_ref()),
                        new_style,
                        PrettyTextEffectCollection(new_effects.into()),
                    ));
                }
                Span::Bundles(bundles) => {
                    let len = effects.len();
                    for effect in effects.iter() {
                        parent_effects.push(effect.clone());
                    }

                    for bundle in bundles.iter() {
                        bundle.with_parent_ref(entity, style, parent_effects);
                    }

                    for _ in 0..len {
                        parent_effects.pop();
                    }
                }
            },
            TextSpanBundle::Effect(effect) => {
                entity.with_child(*effect);
            }
            TextSpanBundle::Event { tag, handler } => match handler {
                Some(handler) => {
                    entity.with_child((tag.clone(), handler.clone()));
                }
                None => {
                    entity.with_child(tag.clone());
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Span {
    Text(Cow<'static, str>),
    Bundles(Cow<'static, [TextSpanBundle]>),
}

#[derive(Component)]
pub(crate) struct PrettyTextEffectCollection(pub Cow<'static, [PrettyTextEffect]>);

#[derive(Debug, Clone)]
pub struct PrettyTextEffect {
    pub tag: Cow<'static, str>,
    pub args: Cow<'static, [Cow<'static, str>]>,
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for PrettyTextEffect {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;
        let tag = &self.tag;
        let args = &self.args;
        tokens.append_all(quote::quote! {
            ::bevy_pretty_text::parser::PrettyTextEffect {
                tag: std::borrow::Cow::Borrowed(#tag),
                args: std::borrow::Cow::Borrowed(&[#(std::borrow::Cow::Borrowed(#args),)*])
            }
        });
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
        token_str.verify_map(|value| value.parse::<f32>().ok()),
        Token::CloseAngle,
    )
    .map(|speed| TextSpanBundle::Effect(TypeWriterEffect::Speed(speed)))
    .parse_next(input)
}

fn pause(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    delimited(
        Token::OpenBracket,
        token_str.verify_map(|value| value.parse::<f32>().ok()),
        Token::CloseBracket,
    )
    .map(|dur| TextSpanBundle::Effect(TypeWriterEffect::Pause(dur)))
    .parse_next(input)
}

fn normal_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    token_str
        .map(|str| TextSpanBundle::Span {
            span: Span::Text(String::from(str).into()),
            style: SpanStyle::Inherit,
            effects: Cow::Borrowed(&[]),
        })
        .parse_next(input)
}

fn styled_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    (
        preceded(Token::BackTick, token_str),
        delimited(Token::Bar, token_str, Token::BackTick),
    )
        .map(|(str, style)| TextSpanBundle::Span {
            span: Span::Text(String::from(str).into()),
            style: SpanStyle::Tag(String::from(style).into()),
            effects: Cow::Borrowed(&[]),
        })
        .parse_next(input)
}

fn styled_effect_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    (
        preceded(
            Token::BackTick,
            repeat(1.., text_components).map(Cow::Owned),
        ),
        opt(preceded(Token::Bar, token_str)),
        Token::BackTick,
        delimited(Token::OpenBracket, effects, Token::CloseBracket),
    )
        .map(|(bundles, style, _, effects)| TextSpanBundle::Span {
            span: Span::Bundles(bundles),
            style: style
                .map(|style| SpanStyle::Tag(String::from(style).into()))
                .unwrap_or(SpanStyle::Inherit),
            effects: Cow::Owned(effects),
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
                    .map(|args| {
                        args.map(|args: Vec<String>| {
                            Cow::Owned(args.into_iter().map(Cow::Owned).collect())
                        })
                        .unwrap_or_default()
                    })
                    .context(StrContext::Label("effect")),
                )
                    .map(|(tag, args)| PrettyTextEffect {
                        tag: tag.into(),
                        args,
                    }),
                multispace1,
            ),
            eof.context(StrContext::Label("effects")),
        )
        .parse_next(&mut &*str),
        _ => Err(ParserError::from_input(input)),
    }
}

fn event(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    delimited(Token::OpenCurly, opt(token_str), Token::CloseCurly)
        .map(|tag| TextSpanBundle::Event {
            tag: TypeWriterEventTag(tag.map(|tag| tag.to_string().into())),
            handler: None,
        })
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
