use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Fade, ComputeFade>, fade)
            .chain()
            .in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Fade>("fade");
}

/// Oscillates the alpha of a glyph.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Fade {
    /// Rate that the fade oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Minimum alpha.
    #[syntax(default = 0.0, "{number}")]
    pub min: f32,

    /// Maximum alpha.
    #[syntax(default = 1.0, "{number}")]
    pub max: f32,

    /// Controls the offset between adjacent glyphs.
    #[syntax(default = 1.0, "{number}")]
    pub offset: f32,
}

#[derive(Default, Component)]
struct ComputeFade;

fn fade(
    time: Res<Time>,
    fades: EffectQuery<&Fade>,
    mut glyphs: Query<(&GlyphIndex, &SpanGlyphOf, &mut GlyphVertices), With<ComputeFade>>,
) {
    for (glyph_index, span_entity, mut vertices) in glyphs.iter_mut() {
        let Ok(fade) = fades.get(span_entity) else {
            continue;
        };

        let time_factor = time.elapsed_secs_wrapped() * fade.frequency;
        let offset = -fade.offset * 0.8 * glyph_index.0 as f32;
        let fade_value = (offset + time_factor * 5.0).sin();
        let t = EaseFunction::SineInOut
            .sample((fade_value + 1.0) / 2.0)
            .unwrap();

        vertices
            .color()
            .for_each(|c| c.set_alpha(fade.min.lerp(fade.max, t)));
    }
}
