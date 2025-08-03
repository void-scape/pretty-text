use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};
use rand::Rng;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphPosition, GlyphSpan};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Shake, ShakeOffset>, shake)
            .chain()
            .in_set(PrettyEffectSet),
    )
    .register_pretty_effect::<Shake>("shake");

    app.register_type::<Shake>();
}

/// Applies random linear motion within a radius.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Shake {
    /// Controls the speed of movement.
    ///
    /// The `intensity` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`GlobalTransform::scale`]s.
    #[syntax(default = 1.0, "{number}")]
    pub intensity: f32,

    /// Maximum displacement from the glyph origin.
    ///
    /// The `radius` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`GlobalTransform::scale`]s.
    #[syntax(default = 1.0, "{number}")]
    pub radius: f32,
}

#[derive(Component)]
struct ShakeOffset {
    t: f32,
    step: f32,
    start: Vec2,
    end: Vec2,
}

impl Default for ShakeOffset {
    fn default() -> Self {
        Self {
            t: 1f32,
            step: 0f32,
            start: Vec2::ZERO,
            end: Vec2::ZERO,
        }
    }
}

fn shake(
    time: Res<Time>,
    shake: EffectQuery<&Shake>,
    mut glyphs: Query<(
        &mut GlyphPosition,
        &mut ShakeOffset,
        &GlyphSpan,
        &GlyphScale,
    )>,
) {
    if glyphs.is_empty() {
        return;
    }

    let mut rng = rand::rng();
    for (mut offset, mut shake_offset, span_entity, scale) in glyphs.iter_mut() {
        for shake in shake.iter(span_entity.0) {
            let new_offset = shake_offset.start.lerp(shake_offset.end, shake_offset.t);
            offset.0 += new_offset.extend(0.);

            shake_offset.t += shake_offset.step * time.delta_secs() * 15f32 * scale.length();
            if shake_offset.t >= 1.0 {
                shake_offset.t = 0.0;
                shake_offset.start = new_offset;
                shake_offset.end = Vec2::new(
                    rng.random_range(-shake.radius..shake.radius),
                    rng.random_range(-shake.radius..shake.radius),
                ) * scale.0;

                let distance = shake_offset.start.distance(shake_offset.end);
                shake_offset.step = if distance > 0.0 {
                    shake.intensity * 2.0 / distance
                } else {
                    1.0
                };
            }
        }
    }
}
