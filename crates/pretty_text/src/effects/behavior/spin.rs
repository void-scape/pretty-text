use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphRotation, GlyphSpan};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Spin, ComputeSpin>, spin)
            .chain()
            .in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Spin>("spin");

    app.register_type::<Spin>();
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
    mut glyphs: Query<(&GlyphSpan, &GlyphIndex, &mut GlyphRotation), With<ComputeSpin>>,
) {
    for (span_entity, glyph_index, mut rotation) in glyphs.iter_mut() {
        let Ok(spin) = spins.get(span_entity) else {
            continue;
        };

        let time_factor =
            time.elapsed_secs_wrapped() * spin.speed + (glyph_index.0 as f32 * spin.offset * -0.05);
        rotation.0 += (time_factor * 2.5) % std::f32::consts::TAU;
    }
}
