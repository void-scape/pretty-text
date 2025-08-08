use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet};
use crate::glyph::{GlyphIndex, GlyphSpan, LocalGlyphScale};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Update, breathe.in_set(PrettyEffectSet))
        .register_pretty_effect::<Breathe>("breathe");

    app.register_type::<Breathe>();
}

/// Oscillates glyph scale.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Breathe {
    /// Rate that the scale oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Scale at the trough of the oscillation.
    #[syntax(default = 0.8, "{number}")]
    pub min: f32,

    /// Scale at the peak of the oscillation.
    #[syntax(default = 1.2, "{number}")]
    pub max: f32,

    /// The amount that the animation of adjacent glyphs is offset.
    #[syntax(default = 1.0, "{number}")]
    pub offset: f32,
}

fn breathe(
    time: Res<Time>,
    breathe: EffectQuery<&Breathe>,
    mut glyphs: Query<(&GlyphSpan, &GlyphIndex, &mut LocalGlyphScale)>,
) {
    for (span_entity, glyph_index, mut scale) in glyphs.iter_mut() {
        let Ok(breathe) = breathe.get(span_entity) else {
            continue;
        };

        let min = Vec2::splat(breathe.min - 1.0);
        let max = Vec2::splat(breathe.max - 1.0);

        let offset = -breathe.offset * 0.08 * glyph_index.0 as f32;
        let t =
            ((offset + time.elapsed_secs_wrapped()) * breathe.frequency * 5.0).sin() / 2.0 + 0.5;
        scale.0 += min.lerp(max, t);
    }
}
