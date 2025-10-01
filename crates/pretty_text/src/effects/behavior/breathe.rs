use std::ops::AddAssign;

use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf, VertexMask};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Breathe, ComputeBreathe>, breathe).in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Breathe>("breathe");
}

/// Oscillates glyph scale.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText, VertexMask)]
#[parser_syntax]
pub struct Breathe {
    /// Rate that the scale oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Minimum scale.
    #[syntax(default = 0.8, "{number}")]
    pub min: f32,

    /// Maximum scale.
    #[syntax(default = 1.2, "{number}")]
    pub max: f32,

    /// Controls the offset between adjacent glyphs.
    #[syntax(default = 1.0, "{number}")]
    pub offset: f32,
}

#[derive(Default, Component)]
struct ComputeBreathe;

fn breathe(
    time: Res<Time>,
    breathe: EffectQuery<(&Breathe, &VertexMask)>,
    mut glyphs: Query<(&SpanGlyphOf, &GlyphIndex, &mut GlyphVertices), With<ComputeBreathe>>,
) {
    for (span_entity, glyph_index, mut vertices) in glyphs.iter_mut() {
        let Ok((breathe, mask)) = breathe.get(span_entity) else {
            continue;
        };

        let min = Vec2::splat(breathe.min - 1.0);
        let max = Vec2::splat(breathe.max - 1.0);

        let offset = -breathe.offset * 0.08 * glyph_index.0 as f32;
        let t =
            ((offset + time.elapsed_secs_wrapped()) * breathe.frequency * 5.0).sin() / 2.0 + 0.5;
        vertices.mask(mask).scale().add_assign(min.lerp(max, t));
    }
}
