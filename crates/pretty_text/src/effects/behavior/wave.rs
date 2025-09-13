use std::ops::AddAssign;

use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf, VertexMask};

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
#[require(PrettyText, VertexMask)]
#[parser_syntax]
pub struct Wave {
    /// Rate that the wave oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Maximum displacement along the y-axis.
    #[syntax(default = 1.0, "{number}")]
    pub height: f32,

    /// Controls the offset between adjacent glyphs.
    #[syntax(default = 1.0, "{number}")]
    pub offset: f32,
}

#[derive(Default, Component)]
struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: EffectQuery<(&Wave, &VertexMask)>,
    mut glyphs: Query<
        (&GlyphIndex, &SpanGlyphOf, &mut GlyphVertices, &GlyphScale),
        // With<ComputeWave>,
    >,
) {
    for (glyph_index, span_entity, mut vertices, scale) in glyphs.iter_mut() {
        let Ok((wave, mask)) = waves.get(span_entity) else {
            continue;
        };

        let scale = scale.0.length();
        let time_factor = time.elapsed_secs_wrapped() * wave.frequency * 1.5;
        let offset = -wave.offset * 0.8 * glyph_index.0 as f32;
        let wave_value = (offset + time_factor * 4.0).sin();
        let t = EaseFunction::Linear
            .sample((wave_value + 1.0) / 2.0)
            .unwrap();

        vertices
            .mask(mask)
            .translation()
            .add_assign(Vec2::new(0f32, t * wave.height * 4.4 * scale));
    }
}
