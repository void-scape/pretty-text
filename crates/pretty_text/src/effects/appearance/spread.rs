use std::ops::AddAssign;

use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};
use pretty_text_parser::Seconds;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSystems, mark_effect_glyphs};
use crate::glyph::{GlyphVertices, SpanGlyphOf, VertexMask};

use super::Appeared;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Spread, ComputeSpread>, spread)
            .chain()
            .in_set(PrettyEffectSystems),
    )
    .register_pretty_effect::<Spread>("spread");
}

/// Animates glyph scale from `min` to `max` to `1.0` over `duration`.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText, VertexMask)]
#[parser_syntax]
pub struct Spread {
    /// Minimum scale.
    #[syntax(default = 0.0, "{number}")]
    pub min: f32,

    /// Maximum scale.
    #[syntax(default = 1.4, "{number}")]
    pub max: f32,

    /// Animation duration in seconds.
    #[syntax(default = Seconds(0.5) => "0.5", "{duration}")]
    pub duration: Seconds,

    /// Time before animation inflects.
    #[syntax(default = Seconds(0.2) => "0.2", "{duration}")]
    pub inflection: Seconds,
}

#[derive(Default, Component)]
struct ComputeSpread;

fn spread(
    spread: EffectQuery<(&Spread, &VertexMask)>,
    mut glyphs: Query<(&SpanGlyphOf, &mut GlyphVertices, &Appeared), With<ComputeSpread>>,
) {
    for (span_entity, mut vertices, appeared) in glyphs.iter_mut() {
        let Ok((spread, mask)) = spread.get(span_entity) else {
            continue;
        };

        if appeared.0 > spread.duration.0 {
            continue;
        }

        let min = spread.min - 1.0;
        let max = spread.max - 1.0;
        let scale = if appeared.0 <= spread.inflection.0 && spread.inflection.0 != 0.0 {
            let t = EaseFunction::SineInOut
                .sample(appeared.0 / spread.inflection.0)
                .unwrap_or(0.0);
            let s = EaseFunction::SineInOut.sample(t).unwrap_or(0.0);
            min + (max - min) * s
        } else {
            let t = EaseFunction::SineInOut
                .sample(
                    (appeared.0 - spread.inflection.0) / (spread.duration.0 - spread.inflection.0),
                )
                .unwrap_or(0.0);
            let s = EaseFunction::SineInOut.sample(t).unwrap_or(0.0);
            max * (1.0 - s)
        };
        vertices.mask(mask).scale().add_assign(scale);
    }
}
