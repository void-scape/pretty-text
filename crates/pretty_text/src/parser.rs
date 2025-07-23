//! # Parser Syntax
//!
//! **Spans** are ranges of text, denoted with backticks: ``"`...`"``.
//!
//! **Modifiers** are a comma separated collection of effects and styles
//! applied to a **span**. They directly follow a **span** and are contained
//! in square brackets: `"[mod1, ...]"`.
//!
//! ## Effects
//!
//! **Effects** are a modifier that optionally take arguments.
//!
//! ``"`Simple effect`[my_effect]"``
//!
//! ``"`Effect with supplied arguments`[my_effect(10, 4.3)]"``
//!
//! ``"`Multiple effects`[my_effect, another_effect]"``
//!
//! ## Styles
//!
//! **Styles** are a modifier, prefixed with `!`.
//!
//! ``"`Simple style`[!my_style]"``
//!
//! ``"`Multiple styles`[!my_style, !another_style]"``
//!
//! # Type Writer Syntax
//!
//! The [`TypeWriter`](crate::type_writer::TypeWriter) has built-in syntax for
//! sequencing:
//! - Pause: `[seconds]`
//!     - ex: `"Pause[1] between"`
//! - Set relative speed: `<mult>`
//!     - ex: `"<2.0>Fast <0.2>Slow"`
//! - Emit [`TypeWriterEvent`]s: `{my_event}`
//!     - ex: `"Emit an {my_event}event"`
//!
//! And with [`pretty!`] and [`pretty2d!`]:
//! - Trigger [`TypeWriterCallback`]s: `{}`
//!     - ex: `pretty!("Trigger a {}callback", |mut commands: Commands| { ... })`
//!
//! [`pretty!`]: https://docs.rs/bevy_pretty_text/latest/bevy_pretty_text/macro.pretty.html
//! [`pretty2d!`]: https://docs.rs/bevy_pretty_text/latest/bevy_pretty_text/macro.pretty2d.html
//!
//! # Usage
//!
//! ```
//! # use bevy::prelude::*;
//! # use pretty_text::parser::*;
#![doc = include_str!("../docs/pretty.txt")]
//! # fn parser() -> Result {
//! # let mut world = World::new();
//! #
//! // Basic usage.
//! world.spawn(PrettyParser::bundle("my pretty text")?);
//!
//! // Static parsing.
//! world.spawn(pretty!("my pretty text"));
//!
//! // Or, save the text for later...
//! let spans = PrettyParser::spans("my pretty text")?;
//!
//! // Which you can be inserted as a component
//! world.spawn(spans);
//! // Or turned into a bundle
//! // world.spawn(spans.into_bundle());
//! # Ok(())
//! # }
//! # parser().unwrap();
//! ```
//!
//! The [`PrettyParser`] and [`PrettyParser2d`] return a result that indicates
//! whether or not the syntax is valid. However, the effects and styles are
//! dynamically constructed at run time, so the parser *will not* fail to parse
//! unregistered modifiers.
//!
//! [`pretty!`] and [`pretty2d!`], similarly, will produce a compiler error if the syntax is
//! invalid, but fails to warn about unregistered modifiers.
//!
//! [`pretty!`]: https://docs.rs/bevy_pretty_text/latest/bevy_pretty_text/macro.pretty.html
//! [`pretty2d!`]: https://docs.rs/bevy_pretty_text/latest/bevy_pretty_text/macro.pretty2d.html
//!
//! # ECS Structure
//!
//! The parser produces [`PrettyTextSpans`], which is a collection of
//! [`TextSpanBundle`]s that represent either raw text spans or special type
//! writer sequencing components.
//!
//! [`PrettyTextSpans`] is a component that, when inserted, populates its entity
//! with a [`Text`] or [`Text2d`] hierarchy. In many cases you will want to turn it into a
//! bundle directly with [`PrettyTextSpans::into_bundle`].
//!
//! ```
//! # use bevy::prelude::*;
//! # use pretty_text::type_writer::*;
//! # use pretty_text::type_writer::hierarchy::*;
#![doc = include_str!("../docs/pretty.txt")]
//! #
//! # let mut world = World::new();
//! // A simple type writer sequence that speeds up in the middle.
//! world.spawn((
//!     TypeWriter::new(30.0),
//!     pretty!("normal speed <2>doubled speed"),
//! ));
//! ```
//!
//! Internally, this is parsed as two spans with a [`TypeWriterCommand`]
//! in between.
//!
//! ```toml
//!               Speed command
//!               ↓↓↓
//! "normal speed <2>doubled speed"
//!  ^^^^^^^^^^^^^
//!  First span      ^^^^^^^^^^^^^
//!                  Second span
//! ```
//!
//! Which can easily be represented with children:
//!
//! ```
//! # use bevy::prelude::*;
//! # use pretty_text::type_writer::*;
//! # use pretty_text::type_writer::hierarchy::*;
//! #
//! # let mut world = World::new();
//! world.spawn((
//!     TypeWriter::new(30.0),
//!     Text::default(),
//!     children![
//!         TextSpan::new("normal speed "), // First span
//!         TypeWriterCommand::Speed(2.0),  // Speed command
//!         TextSpan::new("doubled speed"), // Second span
//!     ]
//! ));
//! ```
//!
//! Note that the spans from [`PrettyTextSpans`] will always be represented
//! as [`TextSpan`] entities, and no text will be placed into the root [`Text`]
//! or [`Text2d`] component.
//!
//! This means that inserting materials or effects directly into the component
//! won't work as expected.
//!
//! ```
//! # use bevy::prelude::*;
//! # use pretty_text::type_writer::*;
//! # use pretty_text::type_writer::hierarchy::*;
#![doc = include_str!("../docs/pretty.txt")]
//! # #[derive(Component, Default)]
//! # struct Shake;
//! #
//! # let mut world = World::new();
//! // This should be avoided!
//! world.spawn((
//!     TypeWriter::new(30.0),
//!     pretty!("normal speed <2>doubled speed"),
//!     Shake::default(),
//! //  ^^^^^ Shake will not apply to any text spans!
//! ));
//! ```

use std::borrow::Cow;
use std::marker::PhantomData;

use bevy::prelude::*;
use bevy::text::TextRoot;

use crate::PrettyText;
use crate::dynamic_effects::PrettyTextEffect;
use crate::style::SpanStyle;
use crate::type_writer::hierarchy::{TypeWriterCallback, TypeWriterCommand, TypeWriterEvent};

/// Dynamically parses pretty text into [`Text`].
///
/// For creating [`Text2d`], see [`PrettyParser2d`].
///
/// See [`parser`](bevy_pretty_text::parser) for syntax documentation.
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::parser::*;
/// #
/// # fn parser() -> Result {
/// # let mut world = World::new();
/// #
/// // Basic usage.
/// world.spawn(PrettyParser::bundle("my pretty text")?);
///
/// // Or, save the text for later...
/// let spans = PrettyParser::spans("my pretty text")?;
///
/// // Which you can be inserted as a component
/// world.spawn(spans);
/// // Or turned into a bundle
/// // world.spawn(spans.into_bundle());
/// # Ok(())
/// # }
/// # parser().unwrap();
/// ```
#[derive(Debug)]
pub struct PrettyParser;

impl PrettyParser {
    /// Parse `pretty_text` into a bundle.
    pub fn bundle(pretty_text: &str) -> Result<impl Bundle, String> {
        Self::spans(pretty_text).map(PrettyTextSpans::into_bundle)
    }

    /// Parse `pretty_text` into a collection of spans.
    pub fn spans(pretty_text: &str) -> Result<PrettyTextSpans<Text>, String> {
        sealed::parse_bundles(pretty_text).map(|spans| PrettyTextSpans {
            spans,
            _root: PhantomData,
        })
    }
}

/// Dynamically parses pretty text into [`Text2d`].
///
/// For creating [`Text`], see [`PrettyParser`].
///
/// See [`parser`](bevy_pretty_text::parser) for syntax documentation.
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::parser::*;
/// #
/// # fn parser() -> Result {
/// # let mut world = World::new();
/// #
/// // Basic usage.
/// world.spawn(PrettyParser2d::bundle("my pretty text")?);
///
/// // Or, save the text for later...
/// let spans = PrettyParser2d::spans("my pretty text")?;
///
/// // Which you can be inserted as a component
/// world.spawn(spans);
/// // Or turned into a bundle
/// // world.spawn(spans.into_bundle());
/// # Ok(())
/// # }
/// # parser().unwrap();
/// ```
#[derive(Debug)]
pub struct PrettyParser2d;

impl PrettyParser2d {
    /// Parse `pretty_text` into a bundle.
    pub fn bundle(pretty_text: &str) -> Result<impl Bundle, String> {
        Self::spans(pretty_text).map(PrettyTextSpans::into_bundle)
    }

    /// Parse `pretty_text` into a collection of spans.
    pub fn spans(pretty_text: &str) -> Result<PrettyTextSpans<Text2d>, String> {
        sealed::parse_bundles(pretty_text).map(|spans| PrettyTextSpans {
            spans,
            _root: PhantomData,
        })
    }
}

/// Marks a viable text root in [`PrettyTextSpans`].
///
/// Implemented by [`Text`] and [`Text2d`].
pub trait Root: std::fmt::Debug + Default + Clone + Reflect + TextRoot + sealed::Sealed {}
impl Root for Text {}
impl Root for Text2d {}

/// Collection of [text spans](TextSpanBundle).
///
/// Inserting [`PrettyTextSpans`] into an entity will insert [`Text2d`] and spawn
/// the text spans as children.
///
/// Use [`PrettyTextSpans::into_bundle`] to convert directly into a bundle.
///
/// You can serialize [`PrettyTextSpans`] with the `serialize` feature. Any
/// [`TypeWriterCallback`]s will be skipped. You can emulate callback behaviour
/// with a [`TypeWriterEvent`] and an [`Observer`] or [`EventReader`].
#[derive(Debug, Clone, Component, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct PrettyTextSpans<R: Root> {
    pub spans: Vec<TextSpanBundle>,
    _root: PhantomData<R>,
}

impl<R: Root> PrettyTextSpans<R> {
    pub fn new(spans: Vec<TextSpanBundle>) -> Self {
        Self {
            spans,
            _root: PhantomData,
        }
    }

    /// Produce a valid text hierarchy bundle.
    pub fn into_bundle(self) -> impl Bundle {
        (
            PrettyText,
            Children::spawn(sealed::TextSpanSpawner::from_vec(self.spans)),
            R::default(),
        )
    }
}

pub(crate) fn pretty_text_spans<R: Root>(
    trigger: Trigger<OnInsert, PrettyTextSpans<R>>,
    mut commands: Commands,
    spans: Query<&PrettyTextSpans<R>>,
) {
    let spans = spans.get(trigger.target()).unwrap();
    commands
        .entity(trigger.target())
        .insert(spans.clone().into_bundle());
}

/// An enumeration of valid bundles in a
/// [type writer sequence](crate::type_writer::hierarchy).
///
/// Useful for storing the entire sequence in a single
/// [collection](PrettyTextSpans).
#[derive(Debug, Clone, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub enum TextSpanBundle {
    /// Span of text with optional modifiers.
    Span {
        /// Range of text.
        span: Span,
        /// One or more [`Modifier`]s applied to `span`.
        mods: Modifiers,
    },
    /// Type writer command.
    Effect(TypeWriterCommand),
    /// Type writer event.
    Event(TypeWriterEvent),
    /// Type writer callback.
    Callback(
        #[cfg_attr(feature = "serialize", serde(skip))]
        #[cfg_attr(feature = "serialize", reflect(skip_serializing))]
        TypeWriterCallback,
    ),
}

impl TextSpanBundle {
    /// Spawn this bundle as a child of `entity`.
    pub fn with_parent(self, entity: &mut EntityCommands) {
        spawn_bundle_with_parent(self, entity);
    }
}

/// Contains a range of text, denoted with backticks: ``"`...`"``.
///
/// This range can contain either raw text or a collection of spans, allowing
/// for recursive parsing.
#[derive(Debug, Clone, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
#[reflect(no_field_bounds)]
pub enum Span {
    /// Raw text data.
    Text(Cow<'static, str>),
    /// Recursive collection of spans.
    Bundles(Vec<TextSpanBundle>),
}

/// A comma separated collection of [effects](crate::dynamic_effects) and [styles](crate::style)
/// directly following a [`Span`], contained within square brackets: `"[mod1, ...]"`.
#[derive(Debug, Default, Clone, Component, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Modifiers(pub Vec<Modifier>);

/// A [style](crate::style) or [effect](crate::dynamic_effects), contained by
/// [`Modifiers`]:
/// - Effect -> `"name[(arg1, ...)]"`
/// - Style  -> `"!name"`
#[derive(Debug, Clone, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub enum Modifier {
    /// A dynamic effect, e.g. `shake`.
    Effect(PrettyTextEffect),
    /// A [pretty style](crate::style).
    Style(SpanStyle),
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for Modifier {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Effect(effect) => {
                quote::quote! {
                    bevy_pretty_text::parser::Modifier::Effect(#effect)
                }
            }
            Self::Style(style) => {
                quote::quote! {
                    bevy_pretty_text::parser::Modifier::Style(#style)
                }
            }
        });
    }
}

fn spawn_bundle_with_parent(bundle: TextSpanBundle, entity: &mut EntityCommands) {
    match bundle {
        TextSpanBundle::Span { span, mods } => match span {
            Span::Text(text) => {
                entity.with_child((PrettyText, TextSpan::new(text), mods));
            }
            Span::Bundles(bundles) => {
                let mut mods = mods.0;
                for bundle in bundles.into_iter() {
                    spawn_bundle_with_parent_recur(bundle, entity, &mut mods);
                }
            }
        },
        TextSpanBundle::Effect(effect) => {
            entity.with_child(effect);
        }
        TextSpanBundle::Event(tag) => {
            entity.with_child(tag);
        }
        TextSpanBundle::Callback(callback) => {
            entity.with_child(callback);
        }
    }
}

fn spawn_bundle_with_parent_recur(
    bundle: TextSpanBundle,
    entity: &mut EntityCommands,
    parent_mods: &mut Vec<Modifier>,
) {
    match bundle {
        TextSpanBundle::Span { span, mods } => match span {
            Span::Text(text) => {
                let mut new_effects = mods.0;
                if !parent_mods.is_empty() {
                    for effect in parent_mods.iter() {
                        new_effects.push(effect.clone());
                    }
                }

                entity.with_child((
                    PrettyText,
                    TextSpan::new(text.as_ref()),
                    Modifiers(new_effects),
                ));
            }
            Span::Bundles(bundles) => {
                let len = mods.0.len();
                parent_mods.extend(mods.0);

                for bundle in bundles.into_iter() {
                    spawn_bundle_with_parent_recur(bundle, entity, parent_mods);
                }

                for _ in 0..len {
                    parent_mods.pop();
                }
            }
        },
        TextSpanBundle::Effect(effect) => {
            entity.with_child(effect);
        }
        TextSpanBundle::Event(tag) => {
            entity.with_child(tag);
        }
        TextSpanBundle::Callback(callback) => {
            entity.with_child(callback);
        }
    }
}

// private parsing module
mod sealed {
    use std::borrow::Cow;

    use bevy::ecs::spawn::SpawnableList;
    use bevy::prelude::*;
    use winnow::combinator::{alt, delimited, fail, opt, preceded, repeat, separated};
    use winnow::error::{ContextError, ErrMode, ParserError, StrContext, StrContextValue};
    use winnow::stream::Stream;
    use winnow::token::take_while;
    use winnow::{Parser, prelude::*};

    use crate::dynamic_effects::PrettyTextEffect;
    use crate::parser::{Modifier, Modifiers};
    use crate::style::SpanStyle;
    use crate::type_writer::hierarchy::{TypeWriterCallback, TypeWriterCommand, TypeWriterEvent};

    use super::{Span, TextSpanBundle};

    #[cfg(not(feature = "serialize"))]
    pub trait Sealed {}
    #[cfg(feature = "serialize")]
    pub trait Sealed: serde::Serialize + serde::Deserialize {}
    impl Sealed for Text {}
    impl Sealed for Text2d {}

    pub struct TextSpanSpawner(std::vec::IntoIter<TextSpanBundle>);

    impl TextSpanSpawner {
        pub fn from_vec(spans: Vec<TextSpanBundle>) -> Self {
            Self(spans.into_iter())
        }
    }

    impl SpawnableList<ChildOf> for TextSpanSpawner {
        fn spawn(self, world: &mut World, entity: Entity) {
            let mut commands = world.commands();
            let mut parent = commands.entity(entity);
            for span in self.0 {
                span.with_parent(&mut parent);
            }
        }

        fn size_hint(&self) -> usize {
            self.0.len()
        }
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

    fn parse_tokens(input: &mut &[Token]) -> ModalResult<Vec<TextSpanBundle>> {
        repeat::<_, _, Vec<_>, _, _>(1.., text_components).parse_next(input)
    }

    fn text_components(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
        alt((
            speed,
            pause,
            normal_text,
            styled_effect_text,
            event,
            fail.context(StrContext::Label("item")),
        ))
        .parse_next(input)
    }

    fn speed(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
        delimited(
            Token::OpenAngle,
            token_str.verify_map(|value| value.parse::<f32>().ok()),
            Token::CloseAngle,
        )
        .map(|speed| TextSpanBundle::Effect(TypeWriterCommand::Speed(speed)))
        .parse_next(input)
    }

    fn pause(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
        delimited(
            Token::OpenBracket,
            token_str.verify_map(|value| value.parse::<f32>().ok()),
            Token::CloseBracket,
        )
        .map(|dur| TextSpanBundle::Effect(TypeWriterCommand::Pause(dur)))
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
        repeat(
            1..,
            alt((
                token_str,
                Token::Bang.map(|_| "!"),
                Token::Comma.map(|_| ","),
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
        .map(|text: RawTextAccumulator| Span::Text(Cow::Owned(text.0.join(""))))
        .parse_next(input)
    }

    fn normal_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
        raw_text
            .map(|span| TextSpanBundle::Span {
                span,
                mods: Modifiers::default(),
            })
            .parse_next(input)
    }

    fn styled_effect_text(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
        (
            preceded(Token::BackTick, repeat(1.., text_components)),
            Token::BackTick,
            delimited(Token::OpenBracket, mods, Token::CloseBracket),
        )
            .map(|(bundles, _, mods)| TextSpanBundle::Span {
                span: Span::Bundles(bundles),
                mods,
            })
            .parse_next(input)
    }

    fn mods(input: &mut &[Token]) -> ModalResult<Modifiers> {
        let mods = separated(
            1..,
            alt((
                preceded(
                    Token::Bang,
                    token_str
                        .verify(|str: &str| !str.trim().contains(char::is_whitespace))
                        .map(|str| String::from(str.trim()))
                        .context(StrContext::Label("modifier"))
                        .context(StrContext::Expected(StrContextValue::Description(
                            "a single modifier, e.g. `shake`",
                        ))),
                )
                .map(|str| Modifier::Style(SpanStyle::Style(Cow::Owned(str)))),
                (
                    token_str
                        .verify(|str: &str| !str.trim().contains(char::is_whitespace))
                        .map(|str| Cow::Owned(String::from(str.trim())))
                        .context(StrContext::Label("modifier"))
                        .context(StrContext::Expected(StrContextValue::Description(
                            "a single modifier, e.g. `shake`",
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
                                    "comma separated list",
                                ))),
                            Token::Comma,
                        )
                        .map_err(ErrMode::cut),
                        Token::CloseParen
                            .map_err(ErrMode::cut)
                            .context(StrContext::Label("closing delimiter")),
                    ))
                    .map(|args: Option<Vec<Cow<'static, str>>>| args.unwrap_or_default()),
                )
                    .map(|(tag, args)| Modifier::Effect(PrettyTextEffect { tag, args })),
            )),
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
            fail.map_err(ErrMode::cut)
                .context(StrContext::Label("text modifiers"))
                .context(StrContext::Expected(StrContextValue::Description(
                    "comma separated list",
                )))
                .parse_next(input)?;
        }

        Ok(Modifiers(mods))
    }

    fn event(input: &mut &[Token]) -> ModalResult<TextSpanBundle> {
        delimited(Token::OpenCurly, opt(token_str), Token::CloseCurly)
            .map(|tag| {
                tag.map(|tag| TextSpanBundle::Event(TypeWriterEvent(tag.to_string())))
                    .unwrap_or_else(|| TextSpanBundle::Callback(TypeWriterCallback::default()))
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
        Bang,
        Comma,
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

    impl Token<'_> {
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
                Self::Bang => "!",
                Self::Comma => ",",
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
        let special_tokens = ['`', '!', '[', ']', '<', '>', '{', '}', '(', ')', ',', '\\'];

        alt((
            "`".map(|_| Token::BackTick),
            "!".map(|_| Token::Bang),
            "[".map(|_| Token::OpenBracket),
            "]".map(|_| Token::CloseBracket),
            "<".map(|_| Token::OpenAngle),
            ">".map(|_| Token::CloseAngle),
            "{".map(|_| Token::OpenCurly),
            "}".map(|_| Token::CloseCurly),
            "(".map(|_| Token::OpenParen),
            ")".map(|_| Token::CloseParen),
            ",".map(|_| Token::Comma),
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
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn assert_ok(str: &str) {
        assert!(PrettyParser::bundle(str).is_ok());
    }

    #[track_caller]
    fn assert_err(str: &str) {
        assert!(PrettyParser::bundle(str).is_err());
    }

    #[test]
    fn valid_parser_syntax() {
        assert_ok("hello, world!");

        assert_ok("`simple style`[!red]");
        assert_ok("`simple style and effect`[!red, shake]");

        assert_ok("simple{tag} tag");
        assert_ok("`simple{tag} tag and effect`[shake]");

        assert_ok("`effect args`[shake(1, \"str\", 9.232)]");

        assert_ok("`recursive `effect`[wave]`[shake]");
        assert_ok("`recursive `effect`[wave] and `style`[!red]`[shake]");

        assert_ok("escaped \\`\\` ticks");
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

        assert_err("{`styled`[!red]}");
    }
}
