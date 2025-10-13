use std::ops::AddAssign;

use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSystems, mark_effect_glyphs};
use crate::glyph::{GlyphIndex, GlyphVertices, SpanGlyphOf, VertexMask};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Bounce, BounceTimeline>, shake)
            .chain()
            .in_set(PrettyEffectSystems),
    )
    .register_pretty_effect::<Bounce>("bounce");
}

/// Applies random linear motion within a radius.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText, VertexMask)]
#[parser_syntax]
pub struct Bounce {
    /// Controls the speed of the whole sequence.
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// The overall hangtime.
    #[syntax(default = 1.0, "{number}")]
    pub hangtime: f32,

    /// The time gap between characters.
    #[syntax(default = 1.0, "{number}")]
    pub spacing: f32,

    /// The delay between subsequent bounces.
    #[syntax(default = 1.0, "{number}")]
    pub delay: f32,
}

#[derive(Component, Default)]
struct BounceTimeline(f32);

fn shake(
    time: Res<Time>,
    shake: EffectQuery<(&Bounce, &VertexMask)>,
    mut glyphs: Query<(
        &mut GlyphVertices,
        &mut BounceTimeline,
        &GlyphIndex,
        &SpanGlyphOf,
        &GlyphScale,
    )>,
) {
    if glyphs.is_empty() {
        return;
    }

    let delta = time.delta_secs();
    for (mut vertices, mut timeline, index, span_entity, scale) in glyphs.iter_mut() {
        let Ok((bounce, mask)) = shake.get(span_entity) else {
            continue;
        };

        let duration_a = 0.3 * bounce.hangtime;
        let duration_b = 0.8;
        let duration_idle = 0.3 * bounce.delay;
        let gap = 0.1 * bounce.spacing;

        let total_duration = duration_a + duration_b + duration_idle + gap;

        let time = ((index.0 as f32 * -gap) + timeline.0).rem_euclid(total_duration);

        if (0f32..duration_a).contains(&time) {
            let curve = EaseFunction::QuadraticOut;
            let t = curve.sample(time / duration_a).unwrap();
            let position = 0f32.lerp(12.0, t);

            vertices
                .mask(mask)
                .translation()
                .add_assign(Vec2::Y * position * scale.0);
        } else if (duration_a..duration_a + duration_b).contains(&time) {
            let progress = time - duration_a;

            let curve = EaseFunction::BounceOut;
            let t = curve.sample(progress / duration_b).unwrap();
            let position = -0f32.lerp(12.0, t) + 12.0;

            vertices
                .mask(mask)
                .translation()
                .add_assign(Vec2::Y * position * scale.0);
        }

        timeline.0 = (timeline.0 + delta * bounce.speed) % total_duration;
    }
}
