use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};
use rand::Rng;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphInstance, GlyphPosition, GlyphSpan};

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
#[require(PrettyText)]
#[parser_syntax]
pub struct Wobble {
    /// Controls the speed of movement.
    ///
    /// The `intensity` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`GlobalTransform::scale`]s.
    #[syntax(default = 1.0, "{number}")]
    pub intensity: f64,

    /// Maximum displacement from the glyph origin.
    ///
    /// The `radius` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`GlobalTransform::scale`]s.
    #[syntax(default = 1.0, "{number}")]
    pub radius: f32,
}

#[derive(Component)]
struct ComputeWobble(f64);

impl Default for ComputeWobble {
    fn default() -> Self {
        Self(rand::rng().random_range(0.0..1_000.0))
    }
}

fn wobble(
    time: Res<Time>,
    wobbles: EffectQuery<&Wobble>,
    mut glyphs: Query<(
        &GlyphInstance,
        &ComputeWobble,
        &mut GlyphPosition,
        &GlyphSpan,
        &GlyphScale,
    )>,
) {
    for (instace, rng, mut offset, span_entity, scale) in glyphs.iter_mut() {
        for wobble in wobbles.iter(span_entity) {
            let time_factor = time.elapsed_secs_f64() * wobble.intensity * 5.0;
            let woffset = instace.0 as f64 * rng.0;
            let x = time_factor.sin() * (time_factor * 1.3 + woffset * 8.0).cos();
            let y = time_factor.cos() * (time_factor * 3.7 + woffset * 3.0).sin();
            offset.0 +=
                (Vec2::new(x as f32, y as f32) * (wobble.radius * 2.6) * scale.0).extend(0.);
        }
    }
}
