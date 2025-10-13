//! Provides a simple abstraction for building [styled text hierarchies](crate::style).
//!
//! Relevant parsing items for [`Text`] and [`Text2d`]:
//! - [`pretty`] and [`pretty2d`]
//! - [`PrettyParser`] and [`PrettyParser2d`]
//!
//! If you are integrating with `bevy_pretty_text` and can not (or don't want to)
//! use the built-in parser, check out the [`ParsedPrettyText`] builder API:
//! - [`PrettyTextUiBuilder`]
//! - [`PrettyText2dBuilder`]
//!
//! # Parser Syntax
//!
//! **Spans** are ranges of text, denoted with square brackets: `"[...]"`.
//!
//! **Styles** are a comma separated collection of styles applied to a **span**.
//! They directly follow a **span** and are contained in parenthesis:
//! `"(style1, style2, ...)"`.
//!
//! ## Styles
//!
//! `"[Simple style](my_style)"`
//!
//! `"[Multiple styles](my_style, another_style)"`
//!
//! ### Registered Styles
//!
//! Styles can be registered in the ECS with [`PrettyStyle`](crate::style::PrettyStyle).
//!
//! A complex set of effects, e.g. `(wave(2), shake(radius=3.4))`, can be collapsed
//! and reused with a registered style:
//!
//! ```no_run
//! # use bevy::prelude::*;
//! # use bevy::color::palettes::css::RED;
//! # use bevy_pretty_text::prelude::*;
//! # let mut world = World::new();
//! world.spawn((
//!     PrettyStyle("my_style"),
//!     effects![
//!         Wave {
//!             frequency: 2.0,
//!             ..Default::default()
//!         },
//!         Shake {
//!             radius: 3.4,
//!             ..Default::default()
//!         },
//!     ]
//! ));
//!
//! // Using `my_style`
//! world.spawn(pretty!("[my text](my_style)"));
//!
//! // Functionally equivalent to:
//! world.spawn(pretty!("[my text](wave(2), shake(radius=3.4))"));
//! ```
//!
//! See [style](crate::style).
//!
//! ### Effect Shorthand
//!
//! **Effects** can be constructed directly without an explicit style entity.
//! Internally, they will be inserted into a new entity with an [`Effects`](crate::effects::Effects)
//! relationship to the span.
//!
//! Effects can be supplied arguments with traditional function syntax. Effects
//! implement [`DynamicEffect`] and are dynamically constructed from a collection
//! of [arguments](crate::style::Arg).
//!
//! Arguments can be positional, as they appear in the effect struct, or named.
//! Positional arguments cannot appear after a named argument.
//!
//! `"[Simple effect](my_effect)"`
//!
//! `"[Effect with supplied arguments](my_effect(10, intensity=4.3))"`
//!
//! Arguments are strings and parsed at run time to their corresponding rust type.
//! See [`ArgParser`] for a description of the argument syntax.
//!
//! [`DynamicEffect`]: crate::effects::dynamic::DynamicEffect
//!
//! # Type Writer Syntax
//!
//! The [`Typewriter`](crate::typewriter::Typewriter) has built-in syntax for
//! sequencing:
//! - Pause: `|seconds|`
//!     - ex: `"Pause|1| between"`
//! - Set relative speed: `<mult>`
//!     - ex: `"<2.0>Fast <0.2>Slow"`
//! - Emit [`TypewriterEvent`]s: `{my_event}`
//!     - ex: `"Emit an {my_event}event"`
//!
//! And with [`pretty`] and [`pretty2d`]:
//! - Trigger [`TypewriterCallback`]s: `{}`
//!     - ex: `pretty!("Trigger a {}callback", |mut commands: Commands| { ... })`
//!
//! # Usage
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
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
//! unregistered styles.
//!
//! [`pretty`] and [`pretty2d`], similarly, will produce a compiler error if the syntax is
//! invalid, but fails to warn about unregistered styles.
//!
//! # ECS Structure
//!
//! The parser produces [`ParsedPrettyText`], which is a collection of
//! [`TextSpanBundle`]s that represent either raw text spans or special type
//! writer sequencing components.
//!
//! [`ParsedPrettyText`] is a component that, when inserted, populates its entity
//! with a [`Text`] or [`Text2d`] hierarchy. In many cases you will want to turn it into a
//! bundle directly with [`ParsedPrettyText::into_bundle`].
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! # let mut world = World::new();
//! // A simple type writer sequence that speeds up in the middle.
//! world.spawn((
//!     Typewriter::new(30.0),
//!     pretty!("normal speed <2>doubled speed"),
//! ));
//! ```
//!
//! This text is parsed as two spans with a [`TypewriterCommand`] in between.
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
//! # use bevy_pretty_text::prelude::*;
//! # use bevy_pretty_text::typewriter::hierarchy::*;
//! # let mut world = World::new();
//! world.spawn((
//!     Typewriter::new(30.0),
//!     Text::default(),
//!     children![
//!         TextSpan::new("normal speed "), // First span
//!         TypewriterCommand::Speed(2.0),  // Speed command
//!         TextSpan::new("doubled speed"), // Second span
//!     ]
//! ));
//! ```
//!
//! Note that the spans from [`ParsedPrettyText`] will always be represented
//! as [`TextSpan`] entities, and no text will be placed into the root [`Text`]
//! or [`Text2d`] component.

use bevy::ptr::MovingPtr;
pub use pretty_text_parser::{
    ArgParser, ParserContext, duration_millis, duration_mins, duration_secs, range, trim,
    tuple_struct,
};

use std::borrow::Cow;
use std::marker::PhantomData;
use std::ops::Range;

use bevy::ecs::spawn::SpawnableList;
use bevy::prelude::*;
use bevy::text::TextRoot;
use pretty_text_parser::{CommandKind, Item, PrettyParserError};

use crate::PrettyText;
use crate::style::{Arg, Style, Styles, Tag};
use crate::typewriter::hierarchy::{TypewriterCallback, TypewriterCommand, TypewriterEvent};

/// Statically parses pretty text into [`Text`].
///
/// For creating [`Text2d`], see [`pretty2d`].
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// #
/// # fn parser() -> Result {
/// # let mut world = World::new();
/// #
/// // Basic usage.
/// world.spawn(pretty!("my pretty text"));
///
/// // Apply a style.
/// world.spawn(pretty!("[my red text](red)"));
///
/// // Make it shake!
/// world.spawn(pretty!("[my shaky text](shake)"));
/// # Ok(())
/// # }
/// # parser().unwrap();
/// ```
///
/// See [module level documentation](crate::parser) for the parser syntax.
pub use pretty_text_macros::pretty;

/// Statically parses pretty text into [`Text2d`].
///
/// For creating [`Text`], see [`pretty`].
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// #
/// # fn parser() -> Result {
/// # let mut world = World::new();
/// #
/// // Basic usage.
/// world.spawn(pretty2d!("my pretty text"));
///
/// // Apply a style.
/// world.spawn(pretty2d!("[my red text](red)"));
///
/// // Make it shake!
/// world.spawn(pretty2d!("[my shaky text](shake)"));
/// # Ok(())
/// # }
/// # parser().unwrap();
/// ```
///
/// See [module level documentation](crate::parser) for the parser syntax.
pub use pretty_text_macros::pretty2d;

/// Dynamically parses pretty text into [`Text`].
///
/// For creating [`Text2d`], see [`PrettyParser2d`].
///
/// See [`parser`](bevy_pretty_text::parser) for syntax documentation.
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # fn parser() -> Result {
/// # let mut world = World::new();
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
/// # assert!(parser().is_ok());
/// ```
#[derive(Debug)]
pub struct PrettyParser;

impl PrettyParser {
    /// Parse `pretty_text` into a bundle.
    #[track_caller]
    pub fn bundle(pretty_text: &str) -> Result<impl Bundle, PrettyParserError> {
        Self::spans(pretty_text).map(ParsedPrettyText::into_bundle)
    }

    /// Parse `pretty_text` into a collection of spans.
    #[track_caller]
    pub fn spans(pretty_text: &str) -> Result<PrettyTextUiSpans, PrettyParserError> {
        pretty_text_parser::parse(pretty_text).map(|items| ParsedPrettyText {
            spans: items.0.into_iter().map(Item::into).collect(),
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
/// # use bevy_pretty_text::prelude::*;
/// # fn parser() -> Result {
/// # let mut world = World::new();
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
/// # assert!(parser().is_ok());
/// ```
#[derive(Debug)]
pub struct PrettyParser2d;

impl PrettyParser2d {
    /// Parse `pretty_text` into a bundle.
    #[track_caller]
    pub fn bundle(pretty_text: &str) -> Result<impl Bundle, PrettyParserError> {
        Self::spans(pretty_text).map(ParsedPrettyText::into_bundle)
    }

    /// Parse `pretty_text` into a collection of spans.
    #[track_caller]
    pub fn spans(pretty_text: &str) -> Result<PrettyText2dSpans, PrettyParserError> {
        pretty_text_parser::parse(pretty_text).map(|items| ParsedPrettyText {
            spans: items.0.into_iter().map(Item::into).collect(),
            _root: PhantomData,
        })
    }
}

/// Marks a viable text root. Implemented for [`Text`] and [`Text2d`].
pub trait Root: std::fmt::Debug + Default + Clone + Reflect + TextRoot + sealed::Sealed {}
impl Root for Text {}
impl Root for Text2d {}

mod sealed {
    pub trait Sealed {}
    impl Sealed for bevy::prelude::Text {}
    impl Sealed for bevy::prelude::Text2d {}
}

/// Collection of parsed [text spans](TextSpanBundle).
///
/// Inserting [`ParsedPrettyText`] into an entity will insert [a text root component](Root)
/// and spawn the text spans as children.
///
/// Use [`ParsedPrettyText::into_bundle`] to convert directly into a bundle.
///
/// You can serialize [`ParsedPrettyText`] with the `serialize` feature. Any
/// [callbacks](TypewriterCallback) will be skipped. You can emulate callback behaviour
/// with a [`TypewriterEvent`] and an [`Observer`].
#[derive(Debug, Clone, Component, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct ParsedPrettyText<R: Root> {
    spans: Vec<TextSpanBundle>,
    #[cfg_attr(feature = "serialize", serde(skip))]
    _root: PhantomData<R>,
}

impl<R: Root> Default for ParsedPrettyText<R> {
    #[track_caller]
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

impl<R: Root> ParsedPrettyText<R> {
    /// Create a new `ParsedPrettyText` with `spans`.
    #[track_caller]
    #[inline]
    pub fn new(spans: Vec<TextSpanBundle>) -> Self {
        Self {
            spans,
            _root: PhantomData,
        }
    }

    /// Append a span.
    #[inline]
    pub fn span(mut self, text: impl AsRef<str>) -> Self {
        self.spans.push(TextSpanBundle::Span {
            span: Span::Text(Cow::Owned(text.as_ref().to_string())),
            styles: Styles::default(),
        });
        self
    }

    /// Append a span with styles.
    #[inline]
    pub fn span_with_styles(
        mut self,
        text: impl AsRef<str>,
        styles: impl IntoIterator<Item = Style>,
    ) -> Self {
        self.spans.push(TextSpanBundle::Span {
            span: Span::Text(Cow::Owned(text.as_ref().to_string())),
            styles: Styles(styles.into_iter().collect()),
        });
        self
    }

    /// Append a [`TypewriterEvent`].
    #[inline]
    pub fn event(mut self, tag: impl AsRef<str>) -> Self {
        self.spans.push(TextSpanBundle::Event(TypewriterEvent(
            tag.as_ref().to_string(),
        )));
        self
    }

    /// Append a [`TypewriterCommand`].
    #[inline]
    pub fn command(mut self, command: TypewriterCommand) -> Self {
        self.spans.push(TextSpanBundle::Command(command));
        self
    }

    /// Append a [`TypewriterCommand::Speed`].
    #[inline]
    pub fn speed_mult(self, mult: f32) -> Self {
        self.command(TypewriterCommand::Speed(mult))
    }

    /// Append a [`TypewriterCommand::Pause`].
    #[inline]
    pub fn pause(self, duration: f32) -> Self {
        self.command(TypewriterCommand::Pause(duration))
    }

    /// Append a [`TypewriterCallback`].
    #[inline]
    pub fn callback<M>(
        mut self,
        callback: impl IntoSystem<(), (), M> + Clone + Send + Sync + 'static,
    ) -> Self {
        self.spans
            .push(TextSpanBundle::Callback(TypewriterCallback::new(callback)));
        self
    }

    /// Append a [`TypewriterCallback`] with mutable world access.
    #[inline]
    pub fn callback_with(
        mut self,
        callback: impl Fn(&mut World) + Clone + Send + Sync + 'static,
    ) -> Self {
        self.spans
            .push(TextSpanBundle::Callback(TypewriterCallback::new_with(
                callback,
            )));
        self
    }

    /// Access the spans.
    #[inline]
    pub fn spans(&self) -> &[TextSpanBundle] {
        &self.spans
    }

    /// Consume and return the underlying spans.
    #[inline]
    pub fn into_spans(self) -> Vec<TextSpanBundle> {
        self.spans
    }

    /// Produce a text hierarchy bundle.
    #[track_caller]
    #[inline]
    pub fn into_bundle(self) -> impl Bundle {
        (
            PrettyText,
            Children::spawn(TextSpanSpawner::new(self.spans)),
            R::default(),
        )
    }
}

/// A [`ParsedPrettyText`] for [`Text2d`].
pub type PrettyText2dSpans = ParsedPrettyText<Text2d>;

/// A [`ParsedPrettyText`] for [`Text`].
pub type PrettyTextUiSpans = ParsedPrettyText<Text>;

pub(crate) fn pretty_text_spans<R: Root>(
    parsed: On<Insert, ParsedPrettyText<R>>,
    mut commands: Commands,
    spans: Query<&ParsedPrettyText<R>>,
) {
    let entity = parsed.event().entity;
    let spans = spans.get(entity).unwrap();
    commands.entity(entity).insert(spans.clone().into_bundle());
}

/// A [`PrettyTextBuilder`] for [`Text`].
pub type PrettyTextUiBuilder = PrettyTextBuilder<Text>;

/// A [`PrettyTextBuilder`] for [`Text2d`].
pub type PrettyText2dBuilder = PrettyTextBuilder<Text2d>;

/// Builds [`ParsedPrettyText`] given a string and optional styles, events,
/// commands, and callbacks.
///
/// Styles are supplied with an index range and events, commands, and callbacks
/// with an index.
#[derive(Debug)]
pub struct PrettyTextBuilder<R: Root> {
    raw: String,
    styles: Vec<(Style, Range<usize>)>,
    events: Vec<(String, usize)>,
    commands: Vec<(TypewriterCommand, usize)>,
    callbacks: Vec<(TypewriterCallback, usize)>,
    _root: PhantomData<R>,
}

impl<R: Root> PrettyTextBuilder<R> {
    /// Create a new [`PrettyTextBuilder`] for `span`.
    #[inline]
    pub fn new(span: impl AsRef<str>) -> Self {
        Self {
            raw: span.as_ref().to_string(),
            styles: Vec::new(),
            events: Vec::new(),
            commands: Vec::new(),
            callbacks: Vec::new(),
            _root: PhantomData,
        }
    }

    /// Create a new [`PrettyTextBuilder`] from `string`.
    #[inline]
    pub fn from_string(string: String) -> Self {
        Self {
            raw: string,
            styles: Vec::new(),
            events: Vec::new(),
            commands: Vec::new(),
            callbacks: Vec::new(),
            _root: PhantomData,
        }
    }

    /// Applies a [`Style`] over `indices`.
    #[inline]
    pub fn style(&mut self, indices: Range<usize>) -> StyleBuilder<'_> {
        self.styles.push((Style::default(), indices));
        let index = self.styles.len() - 1;
        StyleBuilder(&mut self.styles[index].0)
    }

    /// Applies a [`TypewriterEvent`] with `tag` to `index`.
    #[inline]
    pub fn event(&mut self, tag: impl AsRef<str>, index: usize) -> &mut Self {
        self.events.push((tag.as_ref().to_string(), index));
        self
    }

    /// Applies a [`TypewriterCommand`] to `index`.
    #[inline]
    pub fn command(&mut self, command: TypewriterCommand, index: usize) -> &mut Self {
        self.commands.push((command, index));
        self
    }

    /// Applies a [`TypewriterCommand::Speed`] to `index`.
    #[inline]
    pub fn speed_mult(&mut self, mult: f32, index: usize) -> &mut Self {
        self.command(TypewriterCommand::Speed(mult), index)
    }

    /// Applies a [`TypewriterCommand::Pause`] to `index`.
    #[inline]
    pub fn pause(&mut self, duration: f32, index: usize) -> &mut Self {
        self.command(TypewriterCommand::Pause(duration), index)
    }

    /// Applies a [`TypewriterCallback`] to `index`.
    #[inline]
    pub fn callback<M>(
        &mut self,
        callback: impl IntoSystem<(), (), M> + Clone + Send + Sync + 'static,
        index: usize,
    ) -> &mut Self {
        self.callbacks
            .push((TypewriterCallback::new(callback), index));
        self
    }

    /// Applies a [`TypewriterCallback`] with mutable world access to `index`.
    #[inline]
    pub fn callback_with(
        &mut self,
        callback: impl Fn(&mut World) + Clone + Send + Sync + 'static,
        index: usize,
    ) -> &mut Self {
        self.callbacks
            .push((TypewriterCallback::new_with(callback), index));
        self
    }

    /// Builds a new [`ParsedPrettyText`].
    pub fn build(&self) -> ParsedPrettyText<R> {
        use std::collections::BTreeSet;

        let mut boundaries = BTreeSet::new();
        boundaries.insert(0);
        boundaries.insert(self.raw.len());

        for (_, index) in &self.events {
            boundaries.insert(*index);
        }
        for (_, index) in &self.commands {
            boundaries.insert(*index);
        }
        for (_, index) in &self.callbacks {
            boundaries.insert(*index);
        }

        for (_, range) in &self.styles {
            boundaries.insert(range.start);
            boundaries.insert(range.end);
        }

        let boundaries: Vec<usize> = boundaries.into_iter().collect();
        let mut spans = Vec::new();

        for i in 0..boundaries.len() - 1 {
            let start = boundaries[i];
            let end = boundaries[i + 1];

            self.insert_at_position(start, &mut spans);
            if start < end && end <= self.raw.len() {
                let text_slice = &self.raw[start..end];
                if !text_slice.is_empty() {
                    let applicable_styles: Vec<Style> = self
                        .styles
                        .iter()
                        .filter(|(_, style_range)| {
                            style_range.start <= start && end <= style_range.end
                        })
                        .map(|(style, _)| style.clone())
                        .collect();

                    spans.push(TextSpanBundle::Span {
                        span: Span::Text(Cow::Owned(text_slice.to_string())),
                        styles: Styles(applicable_styles),
                    });
                }
            }
        }
        self.insert_at_position(self.raw.len(), &mut spans);

        ParsedPrettyText::new(spans)
    }

    fn insert_at_position(&self, pos: usize, spans: &mut Vec<TextSpanBundle>) {
        for (tag, index) in &self.events {
            if *index == pos {
                spans.push(TextSpanBundle::Event(TypewriterEvent(tag.clone())));
            }
        }

        for (command, index) in &self.commands {
            if *index == pos {
                spans.push(TextSpanBundle::Command(*command));
            }
        }

        for (callback, index) in &self.callbacks {
            if *index == pos {
                spans.push(TextSpanBundle::Callback(callback.clone()));
            }
        }
    }
}

/// Configure a [`Style`] from [`PrettyTextBuilder::style`].
#[derive(Debug)]
pub struct StyleBuilder<'a>(&'a mut Style);

impl StyleBuilder<'_> {
    /// Set the modifier's `tag`.
    #[inline]
    pub fn tag(self, tag: impl AsRef<str>) -> Self {
        self.0.tag = Tag::from(tag.as_ref().to_string());
        self
    }

    /// Append an [`Arg::Positioned`] with `value`.
    #[inline]
    pub fn arg(self, value: impl AsRef<str>) -> Self {
        self.0
            .args
            .push(Arg::Positioned(Cow::Owned(value.as_ref().to_string())));
        self
    }

    /// Append an [`Arg::Named`] with `value`.
    #[inline]
    pub fn named_arg(self, name: impl AsRef<str>, value: impl AsRef<str>) -> Self {
        self.0.args.push(Arg::Named {
            field: Cow::Owned(name.as_ref().to_string()),
            value: Cow::Owned(value.as_ref().to_string()),
        });
        self
    }
}

/// An enumeration of valid bundles produced by the parser.
#[derive(Debug, Clone, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub enum TextSpanBundle {
    /// Span of text with optional styles.
    Span {
        /// Range of text.
        span: Span,
        /// A collection of [styles](Style) applied to `span`.
        styles: Styles,
    },
    /// Type writer command.
    Command(TypewriterCommand),
    /// Type writer event.
    Event(TypewriterEvent),
    /// Type writer callback.
    Callback(
        #[cfg_attr(feature = "serialize", serde(skip))]
        #[cfg_attr(feature = "serialize", reflect(skip_serializing))]
        TypewriterCallback,
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

fn spawn_bundle_with_parent(bundle: TextSpanBundle, entity: &mut EntityCommands) {
    match bundle {
        TextSpanBundle::Span { span, styles } => match span {
            Span::Text(text) => {
                entity.with_child((TextSpan::new(text), styles));
            }
            Span::Bundles(bundles) => {
                let mut styles = styles.0;
                for bundle in bundles.into_iter() {
                    spawn_bundle_with_parent_recur(bundle, entity, &mut styles);
                }
            }
        },
        TextSpanBundle::Command(command) => {
            entity.with_child(command);
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
    parent_styles: &mut Vec<Style>,
) {
    match bundle {
        TextSpanBundle::Span { span, styles } => match span {
            Span::Text(text) => {
                let mut new_styles = styles.0;
                if !parent_styles.is_empty() {
                    for effect in parent_styles.iter() {
                        new_styles.push(effect.clone());
                    }
                }

                entity.with_child((TextSpan::new(text), Styles(new_styles)));
            }
            Span::Bundles(bundles) => {
                let len = styles.0.len();
                parent_styles.extend(styles.0);

                for bundle in bundles.into_iter() {
                    spawn_bundle_with_parent_recur(bundle, entity, parent_styles);
                }

                for _ in 0..len {
                    parent_styles.pop();
                }
            }
        },
        TextSpanBundle::Command(command) => {
            entity.with_child(command);
        }
        TextSpanBundle::Event(tag) => {
            entity.with_child(tag);
        }
        TextSpanBundle::Callback(callback) => {
            entity.with_child(callback);
        }
    }
}

// Spawnable list of `TextSpanBundle`.
struct TextSpanSpawner {
    spans: std::vec::IntoIter<TextSpanBundle>,
}

impl TextSpanSpawner {
    pub fn new(spans: Vec<TextSpanBundle>) -> Self {
        Self {
            spans: spans.into_iter(),
        }
    }
}

impl SpawnableList<ChildOf> for TextSpanSpawner {
    fn spawn(this: MovingPtr<'_, Self>, world: &mut World, entity: Entity) {
        let mut commands = world.commands();
        let mut parent = commands.entity(entity);
        let TextSpanSpawner { spans } = this.read();
        for span in spans {
            span.with_parent(&mut parent);
        }
    }

    fn size_hint(&self) -> usize {
        self.spans.len()
    }
}

impl From<Item<'_>> for TextSpanBundle {
    fn from(value: Item<'_>) -> Self {
        match value {
            Item::Span { span, styles } => Self::Span {
                span: match span {
                    pretty_text_parser::Span::Text(text) => Span::Text(text.into()),
                    pretty_text_parser::Span::Items(items) => {
                        Span::Bundles(items.into_iter().map(Item::into).collect())
                    }
                },
                styles: Styles::new(styles.0.into_iter().map(|style| {
                    Style {
                        tag: style.tag.to_string().into(),
                        args: style
                            .args
                            .into_iter()
                            .map(|arg| match arg {
                                pretty_text_parser::Arg::Positioned(arg) => {
                                    Arg::Positioned(arg.to_string().into())
                                }
                                pretty_text_parser::Arg::Named { name, value } => Arg::Named {
                                    field: name.to_string().into(),
                                    value: value.into(),
                                },
                            })
                            .collect(),
                    }
                })),
            },
            Item::Command { kind, value } => match kind {
                CommandKind::Speed => Self::Command(TypewriterCommand::Speed(value)),
                CommandKind::Pause => Self::Command(TypewriterCommand::Pause(value)),
            },
            Item::Event(tag) => Self::Event(TypewriterEvent(tag.into())),
            Item::Callback => Self::Callback(TypewriterCallback::default()),
        }
    }
}
