//! Built-in text effects for [`bevy_pretty_text`].
//!
//! [See defining custom effects.](pretty_text::dynamic_effects#defining-custom-effects)
//!
//! `bevy_pretty_effects` is an optional dependency, enabled by default in [`bevy_pretty_text`]
//! with the `default_effects` feature.
//!
//! [`bevy_pretty_text`]: https://docs.rs/bevy_pretty_text
//!
//! ## Categories
//!
//! The crate is split broadly into two categories: *behavior* and *appearance*.
//! Behavior effects apply to a glyph for its entire lifetime. Appearance effects
//! apply to a glyph when appearing and disappearing, specifically in the context of a
//! [`Typewriter`](bevy_pretty_text::typewriter::Typewriter).
//!
//! ## Representation
//!
//! Within these categories, there are two different representations of effects:
//! *ECS* and *material*. ECS effects update the position, scale, and rotation of
//! [`Glyph`] entities, whereas material effects set the [`GlyphMaterial`]. A
//! [`Glyph`] can _only have 1 material effect_. However, a [`Glyph`] can have
//! any number of ECS effects!
//!
//! [`GlyphMaterial`]: bevy_pretty_text::material::GlyphMaterial
//!
//! ### Behavior
//!
//! | Component | Tag | ECS Effect | Material Effect |
//! | --------- | --- | :--------: | :-------------: |
//! | [`Wave`] | `wave` | ✅ | ❌ |
//! | [`Shake`] | `shake` | ✅ | ❌ |
//! | [`Wobble`] | `wobble` | ✅ | ❌ |
//! | [`Glitch`] | `glitch` | ❌ | ✅ |
//! | [`Rainbow`] | `rainbow` | ❌ | ✅ |
//!
//! ### Appearance
//!
//! | Component | Tag | ECS Effect | Material Effect |
//! | --------- | --- | :--------: | :-------------: |
//! | [`Scramble`] | `scramble` | ✅ | ❌ |

#![allow(clippy::type_complexity)]
#![warn(missing_debug_implementations, missing_docs, clippy::doc_markdown)]

use bevy::prelude::*;
use bevy_pretty_text::glyph::{Glyph, GlyphSpanEntity};

mod appearance;
mod behavior;

pub use appearance::scramble::{Scramble, ScrambleLifetime, ScrambleSpeed};
pub use behavior::glitch::Glitch;
pub use behavior::rainbow::Rainbow;
pub use behavior::shake::Shake;
pub use behavior::wave::Wave;
pub use behavior::wobble::Wobble;

extern crate pretty_text as bevy_pretty_text;

/// `pretty_text_effects`'s top-level plugin.
///
/// This initializes the built-in text effects for [`bevy_pretty_text`].
#[derive(Debug)]
pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        behavior::plugin(app);
        appearance::plugin(app);
    }
}

/// A [`SystemSet`] for all `pretty_text_effects` core systems.
///
/// Runs in the [`Update`] schedule.
#[derive(Debug, Clone, Copy, SystemSet, Eq, PartialEq, Hash)]
pub struct PrettyEffectSet;

/// This observer triggers whenever a [`Glyph`] is spawned and checks if the
/// glyph's [`GlyphSpanEntity`] has the target `Effect`. If it does, then `Marker`
/// is inserted into the glyph.
pub fn apply_effect_on_glyphs<Effect: Component, Marker: Default + Component>(
    trigger: Trigger<OnAdd, Glyph>,
    mut commands: Commands,
    spans: Query<&GlyphSpanEntity>,
    effects: Query<&Effect>,
) -> Result {
    let span_entity = spans.get(trigger.target())?;
    if effects.get(span_entity.0).is_ok() {
        commands.entity(trigger.target()).insert(Marker::default());
    }
    Ok(())
}
