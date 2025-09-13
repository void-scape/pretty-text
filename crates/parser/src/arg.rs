use std::ops::Range;

use bevy::math::{Vec2, Vec3};
use bevy::reflect::Reflect;
use winnow::Parser;
use winnow::ascii::{multispace0, take_escaped};
use winnow::combinator::*;
use winnow::error::{ErrMode, StrContext};
use winnow::seq;
use winnow::token::{none_of, one_of};

use crate::ParserContext;
use crate::context::Error;

/// Trait for customizing how an [`Arg`](crate::modifier::Arg) is parsed for a
/// [`DynamicEffect`](crate::dynamic_effects::DynamicEffect).
///
/// [`ArgParser`] is implemented for a number of core rust and bevy types that
/// are useful in effects. Here is a table describing how these arguments are
/// translated into rust types:
/// | Rust Type | Input Format | Examples |
/// |-----------|--------------|----------|
/// | `f32` | Floating point number | `3.14`, `42`, `-1.5` |
/// | `f64` | Floating point number | `3.14159`, `42`, `-1.5` |
/// | `u8`, `u16`, `u32`, `u64`, `usize` | Unsigned integer | `42`, `255`, `1024` |
/// | `i8`, `i16`, `i32`, `i64`, `isize` | Signed integer | `42`, `-10`, `0` |
/// | `bool` | Boolean literal | `true`, `false` |
/// | `String` | Quoted string with escape sequences | `"hello"`, `"line\nbreak"`, `"quote\""` |
/// | `Vec2` | Tuple struct with two floats | `vec2(1.0, 2.0)`, `vec2(-3.5, 4.2)` |
/// | `Vec3` | Tuple struct with three floats | `vec3(1.0, 2.0, 3.0)`, `vec3(-1, 0, 1)` |
/// | `Option<T>` | Optional value wrapper | `some(42)`, `none`, `some("text")` |
/// | `Range<T>` | Bounded range | `1..10`, `3.14..6.28`, `0..100` |
/// | Duration (as milliseconds) | Number with optional unit suffix | `1000`, `1s`, `500ms`, `2m` |
/// | Duration (as seconds) | Number with optional unit suffix | `1`, `1s`, `500ms`, `2m` |
/// | Duration (as minutes) | Number with optional unit suffix | `1`, `60s`, `30000ms`, `1m` |
pub trait ArgParser: Sized {
    /// Parse `Self` from `input`.
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self>;
}

/// Trim whitespace surrounding `parser`.
///
/// ### Examples
/// - `  20` -> `20`
/// - `vec2(2, 3)  ` -> `vec2(2, 3)`
pub fn trim<'a, ParseNext, Output>(mut parser: ParseNext) -> impl Parser<&'a str, Output, Error>
where
    ParseNext: Parser<&'a str, Output, Error>,
{
    move |input: &mut &'a str| {
        multispace0(input)?;
        let result = parser.parse_next(input)?;
        multispace0(input)?;
        Ok(result)
    }
}

/// Parse fields of a tuple struct with `inner`.
///
/// Bubbles up errors from `inner` with [`ErrMode::cut`].
///
/// ### Examples
/// - `"vec2(2, 3)"` -> `inner("2, 3")`
/// - `"fixed(8.4)"` -> `inner("8.4")`
pub fn tuple_struct<'a, Inner, Out>(
    ident: &'static str,
    mut inner: Inner,
) -> impl Parser<&'a str, Out, Error>
where
    Inner: Parser<&'a str, Out, Error>,
{
    move |input: &mut &'a str| {
        (
            ident.label("struct ident").expected_str(ident),
            cut_err('(').label("opening delimiter").expected_char('('),
        )
            .parse_next(input)?;
        let inner = inner.parse_next(input).map_err(ErrMode::cut)?;
        cut_err(')')
            .label("closing delimiter")
            .expected_char(')')
            .parse_next(input)?;

        Ok(inner)
    }
}

/// Parse a bounded range from `input`.
///
/// ### Examples
/// - `12.4..19.2`
/// - `2..6`
pub fn range<T: ArgParser>(input: &mut &str) -> winnow::ModalResult<Range<T>> {
    seq! {
        std::ops::Range {
            start: T::parse_arg,
            _: "..",
            end: T::parse_arg
        }
    }
    .parse_next(input)
}

/// A duration expressed in milliseconds.
///
/// ### Examples
/// - `20` -> `20`
/// - `12s` -> `12 * 1000`
/// - `1.5ms` -> `1.5`
/// - `1m` -> `1 * 1000 * 60`
#[derive(Debug, Clone, Copy, PartialEq, Reflect)]
pub struct Milliseconds(pub f32);

impl ArgParser for Milliseconds {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        duration_millis(input).map(|millis| Self(millis))
    }
}

/// Parse a duration from `input` and convert to milliseconds.
///
/// ### Examples
/// - `20` -> `20`
/// - `12s` -> `12 * 1000`
/// - `1.5ms` -> `1.5`
/// - `1m` -> `1 * 1000 * 60`
pub fn duration_millis(input: &mut &str) -> winnow::ModalResult<f32> {
    Ok(duration(input)?.into_millis())
}

/// A duration expressed in seconds.
///
/// ### Examples
/// - `20` -> `20`
/// - `12s` -> `12`
/// - `1.5ms` -> `1.5 / 1000`
/// - `1m` -> `1 * 60`
#[derive(Debug, Clone, Copy, PartialEq, Reflect)]
pub struct Seconds(pub f32);

impl ArgParser for Seconds {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        duration_secs(input).map(|millis| Self(millis))
    }
}

/// Parse a duration from `input` and convert to seconds.
///
/// ### Examples
/// - `20` -> `20`
/// - `12s` -> `12`
/// - `1.5ms` -> `1.5 / 1000`
/// - `1m` -> `1 * 60`
pub fn duration_secs(input: &mut &str) -> winnow::ModalResult<f32> {
    Ok(duration(input)?.into_secs())
}

/// A duration expressed in minutes.
///
/// ### Examples
/// - `20` -> `20`
/// - `12s` -> `12 / 60`
/// - `1.5ms` -> `1.5 / 1000 / 60`
/// - `1m` -> `1`
#[derive(Debug, Clone, Copy, PartialEq, Reflect)]
pub struct Minutes(pub f32);

impl ArgParser for Minutes {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        duration_mins(input).map(|millis| Self(millis))
    }
}

/// Parse a duration from `input` and convert to minutes.
///
/// ### Examples
/// - `20` -> `20`
/// - `12s` -> `12 / 60`
/// - `1.5ms` -> `1.5 / 1000 / 60`
/// - `1m` -> `1`
pub fn duration_mins(input: &mut &str) -> winnow::ModalResult<f32> {
    Ok(duration(input)?.into_mins())
}

struct Duration {
    value: f32,
    unit: Option<Unit>,
}

impl Duration {
    pub fn into_millis(self) -> f32 {
        match self.unit {
            Some(unit) => match unit {
                Unit::Milliseconds => self.value,
                Unit::Seconds => self.value * 1_000f32,
                Unit::Minutes => self.value * 1_000f32 * 60f32,
            },
            None => self.value,
        }
    }

    pub fn into_secs(self) -> f32 {
        match self.unit {
            Some(unit) => match unit {
                Unit::Milliseconds => self.value / 1_000f32,
                Unit::Seconds => self.value,
                Unit::Minutes => self.value * 60f32,
            },
            None => self.value,
        }
    }

    pub fn into_mins(self) -> f32 {
        match self.unit {
            Some(unit) => match unit {
                Unit::Milliseconds => self.value / 1_000f32 / 60f32,
                Unit::Seconds => self.value / 60f32,
                Unit::Minutes => self.value,
            },
            None => self.value,
        }
    }
}

fn duration(input: &mut &str) -> winnow::ModalResult<Duration> {
    seq! {
        Duration {
            value: f32::parse_arg,
            unit: opt(unit)
        }
    }
    .parse_next(input)
}

enum Unit {
    Milliseconds,
    Seconds,
    Minutes,
}

fn unit(input: &mut &str) -> winnow::ModalResult<Unit> {
    alt((
        "ms".map(|_| Unit::Milliseconds),
        "s".map(|_| Unit::Seconds),
        "m".map(|_| Unit::Minutes),
    ))
    .parse_next(input)
}

impl ArgParser for f32 {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        // handle floats before ranges
        if let Some(range_pos) = input.find("..") {
            let prefix = &input[..range_pos];
            if !prefix.is_empty() && prefix.chars().all(|c| c == '-' || c.is_ascii_digit()) {
                return winnow::ascii::dec_int::<_, i32, _>
                    .map(|v| v as f32)
                    .label("float")
                    .parse_next(input);
            }
        }
        winnow::ascii::float.label("float").parse_next(input)
    }
}

impl ArgParser for f64 {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        // handle floats before ranges
        if let Some(range_pos) = input.find("..") {
            let prefix = &input[..range_pos];
            if !prefix.is_empty() && prefix.chars().all(|c| c == '-' || c.is_ascii_digit()) {
                return winnow::ascii::dec_int::<_, i64, _>
                    .label("float")
                    .map(|v| v as f64)
                    .parse_next(input);
            }
        }
        winnow::ascii::float.label("float").parse_next(input)
    }
}

macro_rules! primitive_parser {
    ($ty:ty, $parser:ident) => {
        impl ArgParser for $ty {
            fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
                winnow::ascii::$parser
                    .context(StrContext::Label(stringify!($ty)))
                    .parse_next(input)
            }
        }
    };
    ($ty:ty, $parser:ident => $label:expr) => {
        impl ArgParser for $ty {
            fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
                winnow::ascii::$parser
                    .context(StrContext::Label($label))
                    .parse_next(input)
            }
        }
    };
}

primitive_parser!(u8, dec_uint);
primitive_parser!(u16, dec_uint);
primitive_parser!(u32, dec_uint);
primitive_parser!(u64, dec_uint);
primitive_parser!(usize, dec_uint);

primitive_parser!(i8, dec_int);
primitive_parser!(i16, dec_int);
primitive_parser!(i32, dec_int);
primitive_parser!(i64, dec_int);
primitive_parser!(isize, dec_int);

impl ArgParser for bool {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        alt(("true".map(|_| true), "false".map(|_| false)))
            .label("bool")
            .expected_str("true")
            .expected_str("false")
            .parse_next(input)
    }
}

impl ArgParser for String {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        delimited(
            '"',
            take_escaped(
                none_of(['\\', '"']),
                '\\',
                one_of(['"', '\\', 'n', 't', 'r']),
            ),
            '"',
        )
        .map(str::to_string)
        .label("string")
        .parse_next(input)
    }
}

impl<T: ArgParser> ArgParser for Option<T> {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        alt((
            tuple_struct("some", T::parse_arg.map(Option::Some)),
            "none".map(|_| None),
            fail.label("option")
                .expected_str("none")
                .expected_str("some( /* value */ )"),
        ))
        .parse_next(input)
    }
}

impl ArgParser for Vec2 {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        cut_err(tuple_struct(
            "vec2",
            seq!(
                trim(winnow::ascii::float),
                _: ',',
                trim(winnow::ascii::float),
            ),
        ))
        .map(|(x, y)| Vec2::new(x, y))
        .parse_next(input)
    }
}

impl ArgParser for Vec3 {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        cut_err(tuple_struct(
            "vec3",
            seq!(
                trim(winnow::ascii::float),
                _: ',',
                trim(winnow::ascii::float),
                _: ',',
                trim(winnow::ascii::float),
            ),
        ))
        .map(|(x, y, z)| Vec3::new(x, y, z))
        .parse_next(input)
    }
}
