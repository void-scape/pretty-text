use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphInstance, GlyphPosition, GlyphSpan};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Wave, ComputeWave>, wave)
            .chain()
            .in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Wave>("wave");

    app.register_type::<Wave>();
}

/// Applies oscillating motion to a glyph along the y-axis.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
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

#[derive(Default, Component)]
struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: EffectQuery<&Wave>,
    mut glyphs: Query<
        (&GlyphInstance, &GlyphSpan, &mut GlyphPosition, &GlyphScale),
        With<ComputeWave>,
    >,
) {
    for (instance, span_entity, mut offset, scale) in glyphs.iter_mut() {
        let scale = scale.0.length();

        for wave in waves.iter(span_entity) {
            let time_factor = time.elapsed_secs_f64() * wave.intensity;
            let wave_value = (-(instance.0 as f64) * 0.8 + time_factor * 10.0).sin() * 0.4;
            offset.0.y += wave_value as f32 * wave.max_height * scale * 6f32;
        }
    }
}
