use bevy::prelude::*;
use bevy_pretty_text::dynamic_effects::syntax::ReflectGetDynamicEffectSyntax;
use bevy_pretty_text::glyph::{Glyph, GlyphScale};
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphPosition, GlyphSpanEntity};
use pretty_text_macros::{DynamicEffect, dynamic_effect_docs};

use crate::{PrettyEffectSet, apply_effect_on_glyphs};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Update, wave.in_set(PrettyEffectSet))
        .register_pretty_effect::<Wave>("wave")
        .add_observer(apply_effect_on_glyphs::<Wave, ComputeWave>);

    app.register_type::<Wave>();
}

/// Applies oscillating motion to a glyph along the y-axis.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[reflect(GetDynamicEffectSyntax)]
#[require(PrettyText)]
#[dynamic_effect_docs]
pub struct Wave {
    /// Controls the speed of movement.
    #[syntax(default = 1.0, "{number}")]
    pub intensity: f64,

    /// Maximum displacement along the y-axis from the glyph origin.
    ///
    /// The `max_height` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`GlobalTransform::scale`]s.
    #[syntax(default = 1.0, "{number}")]
    pub max_height: f32,
}

/// Marks glyph as target for the [`Wave`] effect.
#[derive(Default, Component)]
pub struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: Query<&Wave>,
    mut glyphs: Query<(&mut GlyphPosition, &Glyph, &GlyphSpanEntity, &GlyphScale), With<ComputeWave>>,
) -> Result {
    for (mut offset, glyph, span_entity, scale) in glyphs.iter_mut() {
        let wave = waves.get(span_entity.0)?;
        let time_factor = time.elapsed_secs_f64() * wave.intensity;
        let wave_value = (-glyph.position.x as f64 * 0.02 + time_factor * 10.0).sin() * 0.4;
        offset.0.y += wave_value as f32 * wave.max_height * scale.y * 6f32;
    }

    Ok(())
}
