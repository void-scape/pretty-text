#![allow(clippy::type_complexity)]

use bevy::prelude::*;
use bevy_pretty_text::glyph::{Glyph, GlyphSpanEntity};

pub mod scramble;
pub mod shaders;
pub mod shake;
pub mod wave;
pub mod wobble;

extern crate pretty_text as bevy_pretty_text;

pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            shaders::ShadersPlugin,
            scramble::ScramblePlugin,
            shake::ShakePlugin,
            wave::WavePlugin,
            wobble::WobblePlugin,
        ));
    }
}

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
