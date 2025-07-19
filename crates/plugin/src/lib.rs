//! [![crates.io](https://img.shields.io/crates/v/bevy_pretty_text)](https://crates.io/crates/bevy_pretty_text)
//! [![docs.rs](https://docs.rs/bevy_pretty_text/badge.svg)](https://docs.rs/bevy_pretty_text)
//!
//! **Pretty Text** is a Text2d effects library for [Bevy](https://bevyengine.org/).
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
//! ```
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
//!             pretty!("I am [1]<0.8>*sniff*[1]<1.2> very `pretty`[wave, green]![3]<1>"),
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
//! ## Table of contents
//!
//! ### Creating Pretty Text
//! - [Compile-time parsing with `pretty`](pretty_text_macros::pretty)
//! - [Run-time parsing with `PrettyTextParser`](pretty_text::parser::PrettyTextParser)
//!
//! ### TypeWriter
//! - [The `TypeWriter` type](pretty_text::type_writer::TypeWriter)
//! - [Special `TypeWriter` effects](pretty_text::type_writer::hierarchy)
//! - [Controlling text visibility](pretty_text::type_writer::Reveal)
//!
//! ### Parsing
//! - [Syntax](pretty_text::parser)
//!
//! ### Effects
//! - [The `pretty_text_effects` crate](pretty_text_effects)
//! - [ECS effects](pretty_text::dynamic_effects)
//! - [Shader effects](pretty_text::material)
//!
//! ### Style
//! - [The built-in styles](pretty_text::style)
//! - [Creating styles](pretty_text::style::PrettyStyle)

use bevy::prelude::*;

#[cfg(feature = "default_effects")]
pub extern crate pretty_text_effects as effects;

pub use pretty_text::access;
pub use pretty_text::dynamic_effects;
pub use pretty_text::glyph;
pub use pretty_text::material;
pub use pretty_text::parser;
pub use pretty_text::style;
pub use pretty_text::type_writer;
pub use pretty_text_macros::pretty;

pub mod prelude {
    #[doc(hidden)]
    pub use super::PrettyTextPlugin;
    #[doc(hidden)]
    pub use pretty_text::PrettyText;
    #[doc(hidden)]
    pub use pretty_text::parser::PrettyTextParser;
    #[doc(hidden)]
    pub use pretty_text::style::PrettyStyle;
    #[doc(hidden)]
    pub use pretty_text::type_writer::{
        GlyphRevealed, TypeWriter, TypeWriterFinished, TypeWriterMode, WordRevealed,
        hierarchy::TypeWriterEvent,
    };
    #[doc(hidden)]
    pub use pretty_text_macros::pretty;
}

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
