use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};
use rand::Rng;

use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{GlyphSpan, GlyphVertices, VertexMask};

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
#[require(VertexMask)]
#[parser_syntax]
pub struct Shake {
    /// Controls the speed of movement.
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// Maximum displacement from the glyph origin.
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
    shake: EffectQuery<(&Shake, &VertexMask)>,
    mut glyphs: Query<(
        &mut GlyphVertices,
        &mut ShakeOffset,
        &GlyphSpan,
        &GlyphScale,
    )>,
) {
    if glyphs.is_empty() {
        return;
    }

    let mut rng = rand::rng();
    for (mut vertices, mut shake_offset, span_entity, scale) in glyphs.iter_mut() {
        let Ok((shake, mask)) = shake.get(span_entity) else {
            continue;
        };

        let new_offset = shake_offset.start.lerp(shake_offset.end, shake_offset.t);
        vertices
            .mask(mask)
            .iter_mut()
            .for_each(|v| v.translation += new_offset);

        shake_offset.t += shake_offset.step * time.delta_secs() * 15f32 * scale.length();
        if shake_offset.t >= 1.0 {
            shake_offset.t = 0.0;
            shake_offset.start = new_offset;

            let r = shake.radius * 2.0;
            shake_offset.end =
                Vec2::new(rng.random_range(-r..r), rng.random_range(-r..r)) * scale.0;

            let distance = shake_offset.start.distance(shake_offset.end);
            shake_offset.step = if distance > 0.0 {
                shake.speed * 2.0 / distance
            } else {
                1.0
            };
        }
    }
}
