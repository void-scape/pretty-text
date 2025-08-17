use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphSpan, GlyphVertices, VertexMask};

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
#[require(VertexMask)]
#[parser_syntax]
pub struct Wave {
    /// Rate that the wave oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Maximum displacement along the y-axis.
    #[syntax(default = 1.0, "{number}")]
    pub height: f32,
}

#[derive(Default, Component)]
struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: EffectQuery<(&Wave, &VertexMask)>,
    mut glyphs: Query<
        (&GlyphIndex, &GlyphSpan, &mut GlyphVertices, &GlyphScale),
        With<ComputeWave>,
    >,
) {
    for (index, span_entity, mut vertices, scale) in glyphs.iter_mut() {
        let Ok((wave, mask)) = waves.get(span_entity) else {
            continue;
        };

        let scale = scale.0.length();
        let time_factor = time.elapsed_secs_wrapped() * wave.frequency;
        let wave_value = (-(index.0 as f32) * 0.8 + time_factor * 10.0).sin() * 0.4;
        vertices.mask(mask).iter_mut().for_each(|v| {
            v.translation += Vec2::new(0f32, wave_value * wave.height * scale * 6f32);
        });
    }
}
