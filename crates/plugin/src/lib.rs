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
//! bevy_pretty_text = "0.1"
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
//!     // Spawn text.
//!     commands.spawn(pretty!("I am very `pretty`[wave, green]!"));
//!
//!     // Spawn type writer text.
//!     commands
//!         .spawn((
//!             TypeWriter::new(30.),
//!             pretty2d!("I am [1]<0.8>*sniff*[1]<1.2> very `pretty`[wave, green]![3]<1>"),
//!             Transform::from_xyz(0., 200., 0.),
//!         ))
//!         .observe(
//!             |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
//!                 commands
//!                     .entity(trigger.target())
//!                     .insert(TypeWriter::new(30.));
//!             },
//!         );
//! }
//! ```
//!
//! [The repository’s examples] should help you get up to speed on common usage patterns.
//!
//! [The repository’s examples]: https://github.com/void-scape/pretty-text/tree/a0a0a5631b9302d1db292b9e19d6955809835633/crates/plugin/examples
//!
//! # Table of contents
//!
//! ## Creating Pretty Text
//! - [Compile-time parsing with `pretty` and `pretty2d`](crate::pretty)
//! - [Run-time parsing with `PrettyParser` and `PrettyParser2d`](pretty_text::parser::PrettyParser)
//!
//! ## Type Writer
//! - [The `TypeWriter` type](pretty_text::type_writer::TypeWriter)
//! - [Special `TypeWriter` effects](pretty_text::type_writer::hierarchy)
//! - [Controlling text visibility](pretty_text::type_writer::Reveal)
//!
//! ## Parsing
//! - [Syntax](pretty_text::parser)
//! - [ECS Structure](pretty_text::parser#ecs-structure)
//!
//! ## Effects
//! - [The `pretty_text_effects` crate](pretty_text_effects)
//! - [ECS effects](pretty_text::dynamic_effects)
//! - [Shader effects](pretty_text::material)
//!
//! ## Style
//! - [The built-in styles](pretty_text::style)
//! - [Creating styles](pretty_text::style::PrettyStyle)
//!
//! # Feature flags
//!
//! | Flag              | Description                                   | Default feature |
//! | ----------------- | --------------------------------------------- | --------------- |
//! | `default_effects` | Enable the [built-in text effects].           | Yes             |
//! | `serialize`       | Enable serialization for [`PrettyTextSpans`]. | No              |
//!
//! [built-in text effects]: pretty_text_effects
//! [`PrettyTextSpans`]: pretty_text::parser::PrettyTextSpans

#![allow(clippy::too_many_arguments, clippy::type_complexity)]
#![warn(missing_debug_implementations, missing_docs, clippy::doc_markdown)]

use bevy::prelude::*;

#[cfg(feature = "default_effects")]
pub extern crate pretty_text_effects as effects;

pub use pretty_text::access;
pub use pretty_text::dynamic_effects;
pub use pretty_text::glyph;
pub use pretty_text::material;
pub use pretty_text::modifier;
pub use pretty_text::parser;
pub use pretty_text::style;
pub use pretty_text::type_writer;

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
/// world.spawn(pretty!("`my red text`[red]"));
///
/// // Make it shake!
/// world.spawn(pretty!("`my shaky text`[shake]"));
/// # Ok(())
/// # }
/// # parser().unwrap();
/// ```
///
/// See [`parser`] for syntax documentation.
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
/// world.spawn(pretty2d!("`my red text`[red]"));
///
/// // Make it shake!
/// world.spawn(pretty2d!("`my shaky text`[shake]"));
/// # Ok(())
/// # }
/// # parser().unwrap();
/// ```
///
/// See [`parser`] for syntax documentation.
pub use pretty_text_macros::pretty2d;

/// Derive macro for implementing
/// [`DynamicEffect`](pretty_text::dynamic_effects::DynamicEffect).
///
/// # ECS Effect
///
/// ```no_run
#[doc = include_str!("../docs/effect.txt")]
/// ```
///
/// # Material Effect
///
/// ```no_run
#[doc = include_str!("../docs/material.txt")]
/// ```
pub use pretty_text_macros::DynamicEffect;

/// Derive macro for implementing [`GlyphMaterial`](pretty_text::material::GlyphMaterial)
/// and [`DynamicEffect`](pretty_text::material::DynamicEffect).
///
/// ```no_run
#[doc = include_str!("../docs/material.txt")]
/// ```
pub use pretty_text_macros::GlyphMaterial;

/// All `bevy_pretty_text`’s important types and traits.
pub mod prelude {
    pub use super::PrettyTextPlugin;
    pub use pretty_text::PrettyText;
    pub use pretty_text::dynamic_effects::DynamicEffect;
    pub use pretty_text::material::GlyphMaterial;
    pub use pretty_text::parser::{PrettyParser, PrettyParser2d};
    pub use pretty_text::style::PrettyStyle;
    pub use pretty_text::type_writer::{
        DisableCommands, GlyphRevealed, TypeWriter, TypeWriterFinished, TypeWriterMode,
        TypeWriterSet, WordRevealed, hierarchy::TypeWriterEvent,
    };
    pub use pretty_text_macros::{DynamicEffect, GlyphMaterial, pretty, pretty2d};
}

/// `bevy_pretty_text`’s top-level plugin.
///
/// This inserts the core [`pretty_text`] systems and resources then registers
/// [`pretty_text_effects`]'s built-in effects.
#[derive(Debug)]
pub struct PrettyTextPlugin;

impl Plugin for PrettyTextPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            pretty_text::PrettyTextCorePlugin,
            #[cfg(feature = "default_effects")]
            pretty_text_effects::EffectsPlugin,
        ));
    }
}
