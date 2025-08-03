//! Parser implementation for `bevy_pretty_text`.
//!
//! Provides the trait extension [`ParserContext`] for easily adding
//! [`StrContext`](winnow::error::StrContext) for error reporting.

use winnow::combinator::{alt, cut_err, delimited, fail, opt, preceded, repeat, separated};
use winnow::error::{ContextError, ErrMode, ParserError};
use winnow::stream::{Accumulate, Stream};
use winnow::token::take_while;
use winnow::{Parser, prelude::*};

pub use arg::{
    ArgParser, duration_millis, duration_mins, duration_secs, range, trim, tuple_struct,
};
pub use context::ParserContext;

mod arg;
mod context;
pub mod syntax;

pub fn parse<'a>(pretty_text: &'a str) -> Result<PrettyText<'a>, String> {
    tokenize
        .parse(pretty_text)
        .map_err(|err| err.to_string())
        .and_then(|tokens| {
            parse_tokens.parse(&tokens).map_err(|err| {
                pretty_print_token_err(pretty_text, err.input(), err.offset(), err.inner())
            })
        })
}

#[derive(Debug)]
pub struct PrettyText<'a>(pub Vec<Item<'a>>);

#[derive(Debug)]
pub enum Item<'a> {
    Span { span: Span<'a>, styles: Styles<'a> },
    Command { kind: CommandKind, value: f32 },
    Event(&'a str),
    Callback,
}

#[derive(Debug)]
pub enum CommandKind {
    Pause,
    Speed,
}

#[derive(Debug)]
pub enum Span<'a> {
    Text(String),
    Items(Vec<Item<'a>>),
}

#[derive(Debug)]
pub struct Styles<'a>(pub Vec<Style<'a>>);

#[derive(Debug)]
pub struct Style<'a> {
    pub tag: &'a str,
    pub args: Vec<Arg<'a>>,
}

#[derive(Debug)]
pub enum Arg<'a> {
    Positioned(String),
    Named { name: &'a str, value: String },
}

fn parse_tokens<'a>(input: &mut &[Token<'a>]) -> ModalResult<PrettyText<'a>> {
    repeat::<_, _, Vec<_>, _, _>(1.., items)
        .map(PrettyText)
        .parse_next(input)
}

fn items<'a>(input: &mut &[Token<'a>]) -> ModalResult<Item<'a>> {
    alt((
        speed,
        pause,
        normal_text,
        styled_text,
        event,
        fail.label("item"),
    ))
    .parse_next(input)
}

fn speed<'a>(input: &mut &[Token<'a>]) -> ModalResult<Item<'a>> {
    delimited(
        Token::OpenAngle,
        token_str
            .verify_map(|value| value.parse::<f32>().ok())
            .label("argument")
            .expected("float"),
        cut_err(Token::CloseAngle)
            .label("closing delimiter")
            .expected_char('>'),
    )
    .map(|value| Item::Command {
        kind: CommandKind::Speed,
        value,
    })
    .parse_next(input)
}

fn pause<'a>(input: &mut &[Token<'a>]) -> ModalResult<Item<'a>> {
    delimited(
        Token::OpenBracket,
        token_str
            .verify_map(|value| value.parse::<f32>().ok())
            .label("argument")
            .expected("float"),
        cut_err(Token::CloseBracket)
            .label("closing delimiter")
            .expected_char(']'),
    )
    .map(|value| Item::Command {
        kind: CommandKind::Pause,
        value,
    })
    .parse_next(input)
}

#[derive(Default)]
struct RawTextAccumulator<'a>(smallvec::SmallVec<[&'a str; 3]>);

impl<'a> Accumulate<&'a str> for RawTextAccumulator<'a> {
    fn initial(capacity: Option<usize>) -> Self {
        capacity
            .map(|cap| Self(smallvec::SmallVec::with_capacity(cap)))
            .unwrap_or_default()
    }

    fn accumulate(&mut self, acc: &'a str) {
        self.0.push(acc);
    }
}

fn raw_text<'a>(input: &mut &[Token<'a>]) -> ModalResult<Span<'a>> {
    repeat(
        1..,
        alt((
            token_str,
            Token::Comma.map(|_| ","),
            Token::Equals.map(|_| "="),
            Token::OpenParen.map(|_| "("),
            Token::CloseParen.map(|_| ")"),
            (Token::BackSlash, Token::BackTick).map(|_| "`"),
            (Token::BackSlash, Token::OpenBracket).map(|_| "["),
            (Token::BackSlash, Token::CloseBracket).map(|_| "]"),
            (Token::BackSlash, Token::OpenCurly).map(|_| "{"),
            (Token::BackSlash, Token::CloseCurly).map(|_| "}"),
            (Token::BackSlash, Token::OpenAngle).map(|_| "<"),
            (Token::BackSlash, Token::CloseAngle).map(|_| ">"),
        )),
    )
    .map(|text: RawTextAccumulator| Span::Text(text.0.join("")))
    .parse_next(input)
}

fn normal_text<'a>(input: &mut &[Token<'a>]) -> ModalResult<Item<'a>> {
    raw_text
        .map(|span| Item::Span {
            span,
            styles: Styles(Vec::new()),
        })
        .parse_next(input)
}

fn styled_text<'a>(input: &mut &[Token<'a>]) -> ModalResult<Item<'a>> {
    (
        preceded(Token::BackTick, repeat(1.., items)),
        cut_err(Token::BackTick)
            .label("closing delimiter")
            .expected_char('`'),
        delimited(
            Token::OpenBracket
                .label("opening delimiter")
                .expected_char('['),
            styles,
            cut_err(Token::CloseBracket)
                .label("closing delimiter")
                .expected_char(']'),
        ),
    )
        .map(|(items, _, styles)| Item::Span {
            span: Span::Items(items),
            styles,
        })
        .parse_next(input)
}

fn styles<'a>(input: &mut &[Token<'a>]) -> ModalResult<Styles<'a>> {
    let styles = separated(
        1..,
        (
            token_str
                .verify(|str: &str| !str.trim().contains(char::is_whitespace))
                .map(|str| str.trim())
                .label("style")
                .expected("a single style, e.g. `shake`"),
            opt(delimited(
                Token::OpenParen,
                separated(
                    1..,
                    arg.label("argument").expected("comma seperated list"),
                    Token::Comma,
                ),
                cut_err(Token::CloseParen)
                    .label("closing delimiter")
                    .expected_char(')'),
            ))
            .map(|args: Option<Vec<Arg<'a>>>| args.unwrap_or_default()),
        )
            .map(|(tag, args)| Style { tag, args }),
        (
            Token::Comma,
            opt(token_str.verify(|str: &str| str.chars().all(char::is_whitespace))),
        ),
    )
    .parse_next(input)?;

    if input
        .peek_token()
        .is_none_or(|token| !matches!(token, Token::CloseBracket))
    {
        fail.label("styles")
            .expected("comma seperated list")
            .parse_next(input)?;
    }

    Ok(Styles(styles))
}

fn arg<'a>(input: &mut &[Token<'a>]) -> ModalResult<Arg<'a>> {
    alt((
        named,
        positioned.map(Arg::Positioned),
        fail.label("argument")
            .expected("positional argument, `value`, or named argument, `name=value`"),
    ))
    .parse_next(input)
}

fn named<'a>(input: &mut &[Token<'a>]) -> ModalResult<Arg<'a>> {
    (
        token_str.verify(|str: &str| !str.trim().chars().any(char::is_whitespace)),
        Token::Equals,
        cut_err(positioned).label("argument"),
    )
        .map(|(field, _, value)| Arg::Named {
            name: field.trim(),
            value,
        })
        .parse_next(input)
}

fn positioned<'a>(input: &mut &[Token<'a>]) -> ModalResult<String> {
    let first = token_str
        .verify(|str: &str| !str.trim().chars().any(char::is_whitespace))
        .parse_next(input)?;
    if Token::OpenParen.parse_peek(input).is_ok() {
        let inner = delimited(
            Token::OpenParen,
            separated::<_, _, IgnoreAccum, _, _, _, _>(1.., positioned, Token::Comma),
            Token::CloseParen,
        )
        .take()
        .parse_next(input)?;
        Ok(std::iter::once(first)
            .chain(inner.iter().map(Token::as_str))
            .collect::<String>()
            .trim()
            .to_string())
    } else {
        Ok(first.trim().to_string())
    }
}

struct IgnoreAccum;

impl<T> Accumulate<T> for IgnoreAccum {
    fn initial(_: Option<usize>) -> Self {
        Self
    }

    fn accumulate(&mut self, _: T) {}
}

fn event<'a>(input: &mut &[Token<'a>]) -> ModalResult<Item<'a>> {
    delimited(
        Token::OpenCurly,
        opt(token_str.label("event tag").expected("string")),
        cut_err(Token::CloseCurly)
            .label("closing delimiter")
            .expected_char('}'),
    )
    .map(|tag| {
        tag.map(|tag| Item::Event(tag.trim()))
            .unwrap_or_else(|| Item::Callback)
    })
    .parse_next(input)
}

fn token_str<'a>(input: &mut &[Token<'a>]) -> ModalResult<&'a str> {
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
    Comma,
    Equals,
    OpenBracket,
    CloseBracket,
    OpenAngle,
    CloseAngle,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    BackSlash,
}

impl<'a> Token<'a> {
    fn as_str(&self) -> &str {
        match self {
            Self::Text(str) => str,
            _ => self.as_static_str(),
        }
    }

    fn as_static_str(&self) -> &'static str {
        match self {
            Self::Text(_) => "",
            Self::BackTick => "`",
            Self::Comma => ",",
            Self::Equals => "=",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::OpenAngle => "<",
            Self::CloseAngle => ">",
            Self::OpenCurly => "{",
            Self::CloseCurly => "}",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::BackSlash => "\\",
        }
    }
}

fn tokenize<'a>(input: &mut &'a str) -> ModalResult<Vec<Token<'a>>> {
    repeat(0.., token).parse_next(input)
}

fn token<'a>(input: &mut &'a str) -> ModalResult<Token<'a>> {
    let special_tokens = ['`', '[', ']', '<', '>', '{', '}', '(', ')', ',', '=', '\\'];

    alt((
        "`".map(|_| Token::BackTick),
        "[".map(|_| Token::OpenBracket),
        "]".map(|_| Token::CloseBracket),
        "<".map(|_| Token::OpenAngle),
        ">".map(|_| Token::CloseAngle),
        "{".map(|_| Token::OpenCurly),
        "}".map(|_| Token::CloseCurly),
        "(".map(|_| Token::OpenParen),
        ")".map(|_| Token::CloseParen),
        ",".map(|_| Token::Comma),
        "=".map(|_| Token::Equals),
        "\\".map(|_| Token::BackSlash),
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
            .map(|token| token.as_str().chars().count())
            .take(offset)
            .sum();

        for _ in 0..offset {
            arrow_str.push(' ');
        }
        for _ in offset..offset + location.as_str().chars().count() {
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
        assert!(parse(str).is_ok());
    }

    #[track_caller]
    fn assert_err(str: &str) {
        assert!(parse(str).is_err());
    }

    #[test]
    fn valid_parser_syntax() {
        assert_ok("hello, world!");

        assert_ok("`simple style`[red]");
        assert_ok("`simple style and effect`[red, shake]");

        assert_ok("simple{tag} tag");
        assert_ok("`simple{tag} tag and effect`[shake]");

        assert_ok("`effect args`[shake(1, \"str\", 9.232)]");

        assert_ok("`recursive `effect`[wave]`[shake]");
        assert_ok("`recursive `effect`[wave] and `style`[red]`[shake]");

        assert_ok("escaped \\`\\` ticks");

        assert_ok("`modifier delimiters`[red(12, fixed(12, none, (3.4, 1))), shake(vec2(3, 2))]");
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
        assert_err("`empty mods`[]");

        assert_err("`wave and scramble`[wave(1, 20) scramble][2]");

        assert_err("``unclosed`");
        assert_err("`unclosed``");

        assert_err("unclosed{");
        assert_err("unclosed}");

        assert_err("{`styled`[red]}");
    }
}
