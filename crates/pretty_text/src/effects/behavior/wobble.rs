use std::ops::AddAssign;

use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};
use rand::Rng;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf, VertexMask};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Wobble, ComputeWobble>, wobble)
            .chain()
            .in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Wobble>("wobble");

    app.register_type::<Wobble>();
}

/// Applies complex circular motion to a glyph along both x and y axes.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText, VertexMask)]
#[parser_syntax]
pub struct Wobble {
    /// Rate that the wave oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Maximum displacement from the glyph origin.
    #[syntax(default = 1.0, "{number}")]
    pub radius: f32,
}

#[derive(Component)]
struct ComputeWobble(f32);

impl Default for ComputeWobble {
    fn default() -> Self {
        Self(rand::rng().random_range(0.0..1_000.0))
    }
}

fn wobble(
    time: Res<Time>,
    wobbles: EffectQuery<(&Wobble, &VertexMask)>,
    mut glyphs: Query<(
        &GlyphIndex,
        &ComputeWobble,
        &mut GlyphVertices,
        &SpanGlyphOf,
        &GlyphScale,
    )>,
) {
    for (index, rng, mut vertices, span_entity, scale) in glyphs.iter_mut() {
        let Ok((wobble, mask)) = wobbles.get(span_entity) else {
            continue;
        };

        let time_factor = time.elapsed_secs_wrapped() * wobble.frequency * 5.0;
        let woffset = index.0 as f32 * rng.0;
        let x = time_factor.sin() * (time_factor * 1.3 + woffset * 8.0).cos();
        let y = time_factor.cos() * (time_factor * 3.7 + woffset * 3.0).sin();
        vertices
            .mask(mask)
            .translation()
            .add_assign(Vec2::new(x, y) * (wobble.radius * 2.6) * scale.0);
    }
}
