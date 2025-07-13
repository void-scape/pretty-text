use std::borrow::Cow;

use bevy::prelude::*;
use winnow::combinator::{alt, delimited, fail, opt, preceded, repeat, separated};
use winnow::error::{ContextError, ErrMode, ParserError, StrContext, StrContextValue};
use winnow::stream::Stream;
use winnow::token::take_while;
use winnow::{Parser, prelude::*};

use crate::PrettyText;
use crate::bundle::PrettyTextSpans;
use crate::dynamic_effects::{PrettyTextEffect, PrettyTextEffectCollection};
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
        fail.context(StrContext::Label("token")),
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

#[derive(Default)]
struct RawTextAccumulator<'a>(smallvec::SmallVec<[&'a str; 3]>);

impl<'a> winnow::stream::Accumulate<&'a str> for RawTextAccumulator<'a> {
    fn initial(capacity: Option<usize>) -> Self {
        capacity
            .map(|cap| Self(smallvec::SmallVec::with_capacity(cap)))
            .unwrap_or_default()
    }

    fn accumulate(&mut self, acc: &'a str) {
        self.0.push(acc);
    }
}

fn raw_text(input: &mut &[Token]) -> ModalResult<Span> {
    repeat(1.., alt((token_str, Token::Comma.map(|_| ","))))
        .map(|text: RawTextAccumulator| Span::Text(Cow::Owned(text.0.join(""))))
        .parse_next(input)
}

fn normal_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    raw_text
        .map(|span| TextSpanBundle::Span {
            span,
            style: SpanStyle::Inherit,
            effects: Cow::Borrowed(&[]),
        })
        .parse_next(input)
}

fn styled_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
    (
        preceded(Token::BackTick, raw_text),
        delimited(Token::Bar, token_str, Token::BackTick),
    )
        .map(|(span, style)| TextSpanBundle::Span {
            span,
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
    let effects = separated(
        1..,
        (
            token_str
                .verify(|str: &str| !str.trim().contains(char::is_whitespace))
                .map(|str| Cow::Owned(String::from(str.trim())))
                .context(StrContext::Label("effect"))
                .context(StrContext::Expected(StrContextValue::Description(
                    "a single effect, e.g. `shake`",
                ))),
            opt(delimited(
                Token::OpenParen,
                separated(
                    1..,
                    token_str
                        .verify(|str: &str| !str.trim().contains(char::is_whitespace))
                        .map(|str| Cow::Owned(String::from(str.trim())))
                        .context(StrContext::Label("effect argument"))
                        .context(StrContext::Expected(StrContextValue::Description(
                            "comma seperated list",
                        ))),
                    Token::Comma,
                )
                .map_err(ErrMode::cut),
                Token::CloseParen
                    .map_err(ErrMode::cut)
                    .context(StrContext::Label("closing delimiter")),
            ))
            .map(|args: Option<Vec<Cow<'static, str>>>| {
                args.map(|args| Cow::Owned(args)).unwrap_or_default()
            }),
        )
            .map(|(tag, args)| PrettyTextEffect { tag, args }),
        Token::Comma,
    )
    .parse_next(input)?;

    if input
        .peek_token()
        .is_none_or(|token| !matches!(token, Token::CloseBracket))
    {
        fail.map_err(ErrMode::cut)
            .context(StrContext::Label("text effects"))
            .context(StrContext::Expected(StrContextValue::Description(
                "comma seperated list",
            )))
            .parse_next(input)?;
    }

    Ok(effects)
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
    Comma,
    OpenBracket,
    CloseBracket,
    OpenAngle,
    CloseAngle,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
}

impl Token<'_> {
    fn as_str(&self) -> &str {
        match self {
            Self::Text(str) => str,
            Self::BackTick => "`",
            Self::Bar => "|",
            Self::Comma => ",",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::OpenAngle => "<",
            Self::CloseAngle => ">",
            Self::OpenCurly => "{",
            Self::CloseCurly => "}",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
        }
    }
}

fn tokenize<'a>(input: &mut &'a str) -> ModalResult<Vec<Token<'a>>> {
    repeat(0.., token).parse_next(input)
}

fn token<'a>(input: &mut &'a str) -> ModalResult<Token<'a>> {
    let special_tokens = ['`', '|', '[', ']', '<', '>', '{', '}', '(', ')', ','];

    alt((
        "`".map(|_| Token::BackTick),
        "|".map(|_| Token::Bar),
        "[".map(|_| Token::OpenBracket),
        "]".map(|_| Token::CloseBracket),
        "<".map(|_| Token::OpenAngle),
        ">".map(|_| Token::CloseAngle),
        "{".map(|_| Token::OpenCurly),
        "}".map(|_| Token::CloseCurly),
        "(".map(|_| Token::OpenParen),
        ")".map(|_| Token::CloseParen),
        ",".map(|_| Token::Comma),
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

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn assert_ok(str: &str) {
        assert!(PrettyTextParser::parse(str).is_ok());
    }

    #[track_caller]
    fn assert_err(str: &str) {
        assert!(PrettyTextParser::parse(str).is_err());
    }

    #[test]
    fn valid_parser_syntax() {
        assert_ok("hello, world!");

        assert_ok("`simple style|red`");
        assert_ok("`simple style and effect|red`[shake]");

        assert_ok("simple{tag} tag");
        assert_ok("`simple{tag} tag and effect`[shake]");

        assert_ok("`effect args`[shake(1, \"str\", 9.232)]");

        assert_ok("`recursive `effect`[wave]`[shake]");
        assert_ok("`recursive `effect`[wave] and `style|red``[shake]");
    }

    #[test]
    fn invalid_parser_syntax() {
        assert_err("`unclosed");
        assert_err("unclosed`");

        assert_err("`unclosed`[wave");
        assert_err("`unclosed`wave]");
        assert_err("`unclosed`[wave(]");
        assert_err("`unclosed`[wave)]");
        assert_err("`no comma`[wave(1 2)]");
        assert_err("`many commas`[wave(1,, 2)]");

        assert_err("`wave and scrambled`[wave(1, 20) scrambled][2]");

        assert_err("``unclosed`");
        assert_err("`unclosed``");

        assert_err("unclosed{");
        assert_err("unclosed}");

        assert_err("{`styled|red`}");
    }
}
