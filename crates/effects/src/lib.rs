//! Built-in text effects for [`bevy_pretty_text`].
//!
//! `bevy_pretty_effects` is an optional dependency, enabled by default in [`bevy_pretty_text`]
//! with the `default_effects` feature.
//!
//! For creating custom effects, see [`pretty_text::dynamic_effects`].
//!
//! [`PrettyTextEffectAppExt`]: pretty_text::dynamic_effects::PrettyTextEffectAppExt
//! [`DynamicEffect`]: pretty_text::dynamic_effects::DynamicEffect
//! [`bevy_pretty_text`]: https://docs.rs/bevy_pretty_text

#![allow(clippy::type_complexity)]
#![warn(missing_debug_implementations, missing_docs, clippy::doc_markdown)]

use bevy::prelude::*;
use bevy_pretty_text::glyph::{Glyph, GlyphSpanEntity};

mod glitch;
mod scramble;
mod shake;
mod wave;
mod wobble;

pub use glitch::Glitch;
pub use scramble::{Scramble, ScrambleLifetime, ScrambleSpeed};
pub use shake::Shake;
pub use wave::Wave;
pub use wobble::Wobble;

extern crate pretty_text as bevy_pretty_text;

/// `pretty_text_effects`'s top-level plugin.
///
/// This initializes the built-in text effects for [`bevy_pretty_text`].
#[derive(Debug)]
pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        glitch::plugin(app);
        scramble::plugin(app);
        shake::plugin(app);
        wave::plugin(app);
        wobble::plugin(app);
    }
}

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
