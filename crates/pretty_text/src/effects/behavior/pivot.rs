use std::ops::SubAssign;

use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Pivot, ComputePivot>, pivot)
            .chain()
            .in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Pivot>("pivot");
}

/// Shifts the rotation of a glyph from left to right.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Pivot {
    /// Controls the speed of rotation.
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// Maximum rotation applied.
    #[syntax(default = 1.0, "{number}")]
    pub angle: f32,

    /// Controls the offset between adjacent glyphs.
    #[syntax(default = 0.0, "{number}")]
    pub offset: f32,
}

#[derive(Default, Clone, Component)]
struct ComputePivot;

fn pivot(
    time: Res<Time>,
    pivots: EffectQuery<&Pivot>,
    mut glyphs: Query<(&SpanGlyphOf, &GlyphIndex, &mut GlyphVertices), With<ComputePivot>>,
) {
    for (span_entity, glyph_index, mut vertices) in glyphs.iter_mut() {
        let Ok(pivot) = pivots.get(span_entity) else {
            continue;
        };

        let time_factor = time.elapsed_secs_wrapped() * pivot.speed * 4.5
            + glyph_index.0 as f32 * pivot.offset * -0.2;
        vertices
            .rotation()
            .sub_assign(time_factor.sin() * 0.5 * pivot.angle);
    }
}
