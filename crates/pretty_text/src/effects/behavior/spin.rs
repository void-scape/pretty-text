use std::ops::SubAssign;

use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSystems, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Spin, ComputeSpin>, spin)
            .chain()
            .in_set(PrettyEffectSystems),
    )
    .register_pretty_effect::<Spin>("spin");
}

/// Applies constant rotation to a glyph.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Spin {
    /// Controls the speed of rotation.
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// Controls the offset between adjacent glyphs.
    #[syntax(default = 0.0, "{number}")]
    pub offset: f32,
}

#[derive(Default, Component)]
struct ComputeSpin;

fn spin(
    time: Res<Time>,
    spins: EffectQuery<&Spin>,
    mut glyphs: Query<(&SpanGlyphOf, &GlyphIndex, &mut GlyphVertices), With<ComputeSpin>>,
) {
    for (span_entity, glyph_index, mut vertices) in glyphs.iter_mut() {
        let Ok(spin) = spins.get(span_entity) else {
            continue;
        };

        let time_factor =
            time.elapsed_secs_wrapped() * spin.speed + (glyph_index.0 as f32 * spin.offset * -0.05);
        vertices
            .rotation()
            .sub_assign((time_factor * 2.5) % std::f32::consts::TAU);
    }
}
