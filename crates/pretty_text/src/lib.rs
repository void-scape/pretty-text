//! [![crates.io](https://img.shields.io/crates/v/bevy_pretty_text)](https://crates.io/crates/bevy_pretty_text)
//! [![docs.rs](https://docs.rs/bevy_pretty_text/badge.svg)](https://docs.rs/bevy_pretty_text)
//!
//! **Pretty Text** is a text effects library for [Bevy](https://bevyengine.org/).
//!
//! # Getting Started
//!
//! First, add `bevy_pretty_text` to the dependencies in your `Cargo.toml`:
//!  
//! ```toml
//! [dependencies]
//! bevy_pretty_text = "0.2"
//! ```
//!
//! Then, you'll need to add the `PrettyTextPlugin` to your app.
//!
//! ```no_run
//! use bevy::prelude::*;
//! use bevy_pretty_text::prelude::*;
//!
//! # #[allow(clippy::needless_doctest_main)]
//! fn main() {
//!     App::default()
//!         .add_plugins((DefaultPlugins, PrettyTextPlugin))
//!         .run();
//! }
//! ```
//!
//! And then you can make some _pretty text_!
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! #
//! fn spawn_text(mut commands: Commands) {
//!     // Spawn wavy `Text`.
//!     commands.spawn((
//!         Text::new("Hello, World!"),
//!         Wave::default(),
//!     ));
//!
//!     // Use the typewriter.
//!     commands.spawn((
//!         Typewriter::new(30.),
//!         Text2d::new("My text is revealed one glyph at a time"),
//!         Transform::from_xyz(0., 200., 0.),
//!     ));
//!
//!     // Spawn a style entity.
//!     commands.spawn((
//!         PrettyStyle("my_style"),
//!         TextColor(Color::WHITE),
//!         effects![
//!             Shake::default(),
//!             Wave::default(),
//!         ],
//!     ));
//!
//!     // Parse rich text and use custom style.
//!     commands.spawn((
//!         pretty!("I am [1]<0.8>*sniff*[1]<1.2> very [pretty](my_style)![3]<1>"),
//!         Transform::from_xyz(0.0, -200.0, 0.0),
//!     ));
//! }
//! ```
//!
//! [The repository examples] should help you get up to speed on common usage patterns.
//!
//! [The repository examples]: https://github.com/void-scape/pretty-text/tree/f7fd940f0251af7ece31411d5bd078e7e36ae18e/examples/src/bin
//!
//! # Table of contents
//!
//! ## Creating Pretty Text
//! - [Compile-time parsing with `pretty` and `pretty2d`](crate::parser::pretty)
//! - [Run-time parsing with `PrettyParser` and `PrettyParser2d`](crate::parser::PrettyParser)
//!
//! ## Type Writer
//! - [`Typewriter`](crate::typewriter::Typewriter)
//! - [Special `Typewriter` sequencing](crate::typewriter::hierarchy)
//!
//! ## Parsing
//! - [Syntax](crate::parser)
//! - [ECS Structure](crate::parser#ecs-structure)
//!
//! ## Effects
//! - [Built-in effects](mod@crate::effects)
//! - [ECS effects](crate::effects::dynamic#ecs-effects)
//! - [Shader effects](crate::effects::material)
//!
//! ## Style
//! - [The built-in styles](crate::style#default-styles)
//! - [Registering styles](crate::style::PrettyStyle)
//!
//! # Feature flags
//!
//! | Flag | Description | Default feature |
//! | ---- | ----------- | :-------------: |
//! | `serialize` | Enable serialization for [`ParsedPrettyText`]. | ❌ |
//!
//! [`ParsedPrettyText`]: crate::parser::ParsedPrettyText

#![allow(clippy::too_many_arguments, clippy::type_complexity)]
#![warn(missing_debug_implementations, missing_docs, clippy::doc_markdown)]

use bevy::prelude::*;

extern crate self as bevy_pretty_text;

pub mod effects;
pub mod glyph;
pub mod parser;
pub mod render;
pub mod style;
pub mod typewriter;

/// All `bevy_pretty_text`’s important types and traits.
pub mod prelude {
    pub use super::parser::{ParsedPrettyText, PrettyParser, PrettyParser2d, pretty, pretty2d};
    pub use super::style::{PrettyStyle, PrettyStyleSystems, Style2dWriter, StyleUiWriter};
    pub use super::typewriter::{
        Char, DisableCommands, DisableEvents, FinishTypewriter, PauseTypewriter, Revealed,
        ShortCircuitTypewriter, Typewriter, TypewriterFinished, TypewriterIndex, TypewriterSet,
        Word, hierarchy::TypewriterEvent,
    };
    pub use super::{PrettyText, PrettyTextPlugin};

    pub use super::effects::appearance::{
        FadeIn, Scramble, ScrambleLifetime, ScrambleSpeed, Spread,
    };
    pub use super::effects::behavior::{
        Bounce, Breathe, Fade, Glitch, Pivot, Rainbow, Shake, Spin, Wave, Wobble,
    };
    pub use super::effects::dynamic::{DynamicEffect, PrettyTextEffectAppExt};
    pub use super::effects::material::{
        GlyphMaterial, PrettyTextMaterial, PrettyTextMaterialAppExt,
    };
    pub use super::effects::{EffectOf, EffectQuery, Effects};
    pub use crate::effects;
}

/// Top level text component.
///
/// [`PrettyText`] enables the text from a text hierarchy to be converted into
/// [`Glyph`](glyph::Glyph)s.
#[derive(Debug, Default, Component, Reflect)]
pub struct PrettyText;

/// `bevy_pretty_text`’s top-level plugin.
///
/// Initializes the built-in styles and effects systems and resources.
#[derive(Debug)]
pub struct PrettyTextPlugin;

impl Plugin for PrettyTextPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            render::plugin,
            glyph::GlyphPlugin,
            typewriter::TypewriterPlugin,
            style::StylePlugin,
            effects::EffectsPlugin,
        ))
        .add_observer(parser::pretty_text_spans::<Text>)
        .add_observer(parser::pretty_text_spans::<Text2d>);
    }
}
