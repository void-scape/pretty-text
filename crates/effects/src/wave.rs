use bevy::prelude::*;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphOrigin, GlyphSpanEntity};
use pretty_text::{PrettyText, PrettyTextSystems};
use pretty_text_macros::TextEffect;

use crate::apply_effect_on_glyphs;

pub struct WavePlugin;

impl Plugin for WavePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(FixedUpdate, wave.before(PrettyTextSystems::GlyphPosition))
            .register_pretty_effect::<Wave>("wave")
            .add_observer(apply_effect_on_glyphs::<Wave, ComputeWave>);

        app.register_type::<Wave>();
    }
}

#[derive(Debug, Clone, Copy, Component, Reflect, TextEffect)]
#[require(PrettyText)]
pub struct Wave {
    pub intensity: f32,
    pub max_height: f32,
}

impl Default for Wave {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            max_height: 20.0,
        }
    }
}

#[derive(Default, Component)]
struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: Query<&Wave>,
    mut glyphs: Query<(&mut GlyphOffset, &GlyphOrigin, &GlyphSpanEntity), With<ComputeWave>>,
) -> Result {
    for (mut offset, origin, span_entity) in glyphs.iter_mut() {
        let wave = waves.get(span_entity.0)?;
        let time_factor = time.elapsed_secs() * wave.intensity;
        let wave_value = (origin.x * 0.02 + time_factor * 10.0).sin() * 0.4;
        offset.0.y += wave_value * wave.max_height * time.delta_secs() * 60.0;
    }

    Ok(())
}
