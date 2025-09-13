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
//!             Typewriter::new(30.),
//!             pretty2d!("I am [1]<0.8>*sniff*[1]<1.2> very `pretty`[wave, green]![3]<1>"),
//!             Transform::from_xyz(0., 200., 0.),
//!         ))
//!         .observe(
//!             |trigger: Trigger<TypewriterFinished>, mut commands: Commands| {
//!                 commands
//!                     .entity(trigger.target())
//!                     .insert(Typewriter::new(30.));
//!             },
//!         );
//! }
//! ```
//!
//! [The repository’s examples] should help you get up to speed on common usage patterns.
//!
//! [The repository’s examples]: TODO
//!
//! # Table of contents
//!
//! ## Creating Pretty Text
//! - [Compile-time parsing with `pretty` and `pretty2d`](TODO)
//! - [Run-time parsing with `PrettyParser` and `PrettyParser2d`](crate::parser::PrettyParser)
//!
//! ## Type Writer
//! - [The `Typewriter` type](crate::typewriter::Typewriter)
//! - [Special `Typewriter` effects](crate::typewriter::hierarchy)
//! - [Controlling text visibility](crate::typewriter::Reveal)
//!
//! ## Parsing
//! - [Syntax](crate::parser)
//! - [ECS Structure](crate::parser#ecs-structure)
//!
//! ## Effects
//! - [Built-in effects](crate::effects)
//! - [ECS effects](crate::dynamic_effects)
//! - [Shader effects](crate::material)
//!
//! ## Style
//! - [The built-in styles](crate::style)
//! - [Registering styles](crate::style::PrettyStyle)
//!
//! # Feature flags
//!
//! | Flag              | Description                                   | Default feature |
//! | ----------------- | --------------------------------------------- | --------------- |
//! | `serialize`       | Enable serialization for [`ParsedPrettyText`].| No              |
//!
//! [`ParsedPrettyText`]: pretty_text::parser::ParsedPrettyText

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
    pub use super::style::{PrettyStyle, PrettyStyleSet, Style2dWriter, StyleUiWriter};
    pub use super::typewriter::{
        DisableCommands, GlyphRevealed, Typewriter, TypewriterFinished, TypewriterMode,
        TypewriterSet, WordRevealed, hierarchy::TypewriterEvent,
    };
    pub use super::{PrettyText, PrettyTextPlugin};

    pub use super::effects::appearance::{Scramble, ScrambleLifetime, ScrambleSpeed, Spread};
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
        render::plugin(app);
        app.add_plugins((
            glyph::GlyphPlugin,
            typewriter::TypewriterPlugin,
            style::StylePlugin,
            effects::EffectsPlugin,
        ))
        .add_observer(parser::pretty_text_spans::<Text>)
        .add_observer(parser::pretty_text_spans::<Text2d>)
        .register_type::<PrettyText>();
    }
}

#[cfg(test)]
mod test {
    use bevy::{ecs::system::RunSystemOnce, prelude::*};

    use crate::PrettyText;

    pub fn prepare_app() -> App {
        let mut app = App::new();

        app.add_plugins((
            MinimalPlugins,
            // Required for the text systems
            AssetPlugin::default(),
            bevy::render::texture::ImagePlugin::default(),
            bevy::text::TextPlugin,
            bevy::image::TextureAtlasPlugin,
            //
            super::PrettyTextPlugin,
        ));

        app.finish();
        app.cleanup();

        app
    }

    pub fn prepare_app_with(f: impl FnOnce(&mut App)) -> App {
        let mut app = App::new();

        app.add_plugins((
            MinimalPlugins,
            // Required for the text systems
            AssetPlugin::default(),
            bevy::render::texture::ImagePlugin::default(),
            bevy::text::TextPlugin,
            bevy::image::TextureAtlasPlugin,
            //
            super::PrettyTextPlugin,
        ));

        f(&mut app);

        app.finish();
        app.cleanup();

        app
    }

    pub fn run<F: IntoSystem<(), O, M>, O, M>(app: &mut App, system: F) -> O {
        let world = app.world_mut();
        world.run_system_once(system).unwrap()
    }

    pub fn run_tests(
        mut app: impl FnMut() -> App,
        mut test: impl FnMut(&mut App, Entity, &'static str),
    ) {
        fn runner<B: Bundle>(
            app: &mut impl FnMut() -> App,
            test: &mut impl FnMut(&mut App, Entity, &'static str),
            spans: impl Iterator<Item = (&'static str, B)>,
        ) {
            for (str, bundle) in spans {
                let mut app = app();
                let id = app.world_mut().spawn((PrettyText, bundle)).id();
                test(&mut app, id, str);
            }
        }

        runner(&mut app, &mut test, roots());
        runner(&mut app, &mut test, spans());
        runner(&mut app, &mut test, many_spans());
    }

    pub fn roots() -> impl Iterator<Item = (&'static str, impl Bundle)> {
        [
            "!@#$%^&*()_+-=[]{}\\|/><.,;'\"`~",
            "normal_123",
            "¯\\_(ツ)_/¯",
            "( ಠ ͜ʖರೃ)",
            "T̴̰̦̩̲̬̥̘̤̦̤̫̟̭̝̩̯̖̪̱̱̤̱̞̰̤̥̙̜̯̍̂̄̈́̀̈́̑̈́͌̉̇̂̓̓̍̋̄̽̓̾̐̇̊͊̈́̕͘͜͜͝h̶̡̧̨̡̧̙̳̰̼̻̗̰̪̻̝̹̲̙̩̭̻̤̼̺̳̰̘̺̟̺̫̯̯̪̲̳̖̰̤̼̤̞̘̥̗̜̗̬̹͎͓̻̯̫̯̗̣͎̭̥̞̦̼̮͉̯̭̟̦͈̪͇̹̩̯̰̝̯̺̳̀͑̇̓̈́̆͗̃̈̍̈́͊̈́͒̍̋̂̒͗̅̋͒͋̂̅̈́̒̅͌̃̀̔̊̆̿̐̾̏̋͊̇̐̄̂̒̊̾̔̍̂̄̈́̈́̓̌͗̑̒̍̇̆̂́̀̈́̈͗͛͌́̇̆̾̾̽̽́̊́̏̿̈́̒̽͗̔̈̎͂͂́͘̚̚̚͜͜͠͝͝͝͠ͅi̴̧̧̢̡̛̛̩̰̱̯̠̞̖̼͇̦̳͔͈̳̬̭̖̱̺̤̪̹͚̯͓̘͈̗̰̯̭̦̪̺͓̤̹",
        ]
        .into_iter()
        .map(|str| (str, Text2d::new(str)))
    }

    fn spans() -> impl Iterator<Item = (&'static str, impl Bundle)> {
        [
            "!@#$%^&*()_+-=[]{}\\|/><.,;'\"`~",
            "normal_123",
            "¯\\_(ツ)_/¯",
            "( ಠ ͜ʖರೃ)",
            "T̴̰̦̩̲̬̥̘̤̦̤̫̟̭̝̩̯̖̪̱̱̤̱̞̰̤̥̙̜̯̍̂̄̈́̀̈́̑̈́͌̉̇̂̓̓̍̋̄̽̓̾̐̇̊͊̈́̕͘͜͜͝h̶̡̧̨̡̧̙̳̰̼̻̗̰̪̻̝̹̲̙̩̭̻̤̼̺̳̰̘̺̟̺̫̯̯̪̲̳̖̰̤̼̤̞̘̥̗̜̗̬̹͎͓̻̯̫̯̗̣͎̭̥̞̦̼̮͉̯̭̟̦͈̪͇̹̩̯̰̝̯̺̳̀͑̇̓̈́̆͗̃̈̍̈́͊̈́͒̍̋̂̒͗̅̋͒͋̂̅̈́̒̅͌̃̀̔̊̆̿̐̾̏̋͊̇̐̄̂̒̊̾̔̍̂̄̈́̈́̓̌͗̑̒̍̇̆̂́̀̈́̈͗͛͌́̇̆̾̾̽̽́̊́̏̿̈́̒̽͗̔̈̎͂͂́͘̚̚̚͜͜͠͝͝͝͠ͅi̴̧̧̢̡̛̛̩̰̱̯̠̞̖̼͇̦̳͔͈̳̬̭̖̱̺̤̪̹͚̯͓̘͈̗̰̯̭̦̪̺͓̤̹",
        ]
        .into_iter()
        .map(|str| (str, (Text2d::default(), children![TextSpan::new(str)])))
    }

    fn many_spans() -> impl Iterator<Item = (&'static str, impl Bundle)> {
        [
            (
                "!@#$%^&*()_+-=[]{}\\|/><.,;'\"`~",
                ("!@#$%^&*()_+-=", "[]{}\\|/><.,;'\"`~"),
            ),
            ("normal_123", ("normal", "_123")),
            ("¯\\_(ツ)_/¯", ("¯\\_(ツ", ")_/¯")),
            ("( ಠ ͜ʖರೃ)", ("( ಠ", " ͜ʖರೃ)")),
            ("h̶̡̧̨̡̧̙̳̰̼̻̗̰̪̻̝̹̲̙̩̭̻̤̼̺̳̰̘̺̟̺̫̯̯̪̲̳̖̰̤̼̤̞̘̥̗̜̗̬̹͎͓̻̯̫̯̗̣͎̭̥̞̦̼̮͉̯̭̟̦͈̪͇̹̩̯̰̝̯̺̳̀͑̇̓̈́̆͗̃̈̍̈́͊̈́͒̍̋̂̒͗̅̋͒͋̂̅̈́̒̅͌̃̀̔̊̆̿̐̾̏̋͊̇̐̄̂̒̊̾̔̍̂̄̈́̈́̓̌͗̑̒̍̇̆̂́̀̈́̈͗͛͌́̇̆̾̾̽̽́̊́̏̿̈́̒̽͗̔̈̎͂͂́͘̚̚̚͜͜͠͝͝͝͠ͅi̴̧̧̢̡̛̛̩̰̱̯̠̞̖̼͇̦̳͔͈̳̬̭̖̱̺̤̪̹͚̯͓̘͈̗̰̯̭̦̪̺͓̤̹T̴̰̦̩̲̬̥̘̤̦̤̫̟̭̝̩̯̖̪̱̱̤̱̞̰̤̥̙̜̯̍̂̄̈́̀̈́̑̈́͌̉̇̂̓̓̍̋̄̽̓̾̐̇̊͊̈́̕͘͜͜͝", ("h̶̡̧̨̡̧̙̳̰̼̻̗̰̪̻̝̹̲̙̩̭̻̤̼̺̳̰̘̺̟̺̫̯̯̪̲̳̖̰̤̼̤̞̘̥̗̜̗̬̹͎͓̻̯̫̯̗̣͎̭̥̞̦̼̮͉̯̭̟̦͈̪͇̹̩̯̰̝̯̺̳̀͑̇̓̈́̆͗̃̈̍̈́͊̈́͒̍̋̂̒͗̅̋͒͋̂̅̈́̒̅͌̃̀̔̊̆̿̐̾̏̋͊̇̐̄̂̒̊̾̔̍̂̄̈́̈́̓̌͗̑̒̍̇̆̂́̀̈́̈͗͛͌́̇̆̾̾̽̽́̊́̏̿̈́̒̽͗̔̈̎͂͂́͘̚̚̚͜͜͠͝͝͝͠ͅi̴̧̧̢̡̛̛̩̰̱̯̠̞̖̼͇̦̳͔͈̳̬̭̖̱̺̤̪̹͚̯͓̘͈̗̰̯̭̦̪̺͓̤̹", "T̴̰̦̩̲̬̥̘̤̦̤̫̟̭̝̩̯̖̪̱̱̤̱̞̰̤̥̙̜̯̍̂̄̈́̀̈́̑̈́͌̉̇̂̓̓̍̋̄̽̓̾̐̇̊͊̈́̕͘͜͜͝")),
        ]
        .into_iter()
        .map(|(len, (f, s))| {
            (
                len,
                (
                    Text2d::default(),
                    children![TextSpan::new(f), TextSpan::new(s)],
                ),
            )
        })
    }
}
