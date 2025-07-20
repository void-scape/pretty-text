use bevy::prelude::*;
use bevy_pretty_text::glyph::{GlyphScale, GlyphSystems};
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphSpanEntity};
use pretty_text_macros::DynamicEffect;
use rand::Rng;

use crate::apply_effect_on_glyphs;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Update, shake.before(GlyphSystems::Position))
        .register_pretty_effect::<Shake>("shake")
        .add_observer(apply_effect_on_glyphs::<Shake, ShakeOffset>);

    app.register_type::<Shake>();
}

/// Applies random linear motion within a radius.
///
/// ```
#[doc = include_str!("../docs/header.txt")]
/// // Parsed usage
/// world.spawn(pretty!("`my text`[shake(1, 1)]"));
/// world.spawn(PrettyTextParser::bundle("`my text`[shake(1, 1)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text2d::new("my text"),
///     Shake {
///         intensity: 1.0,
///         radius: 1.0,
///     },
/// ));
#[doc = include_str!("../docs/footer.txt")]
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
pub struct Shake {
    /// Controls the speed of movement.
    ///
    /// The `intensity` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`Transform::scale`]s.
    pub intensity: f32,

    /// Maximum displacement from the glyph origin.
    ///
    /// The `radius` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`Transform::scale`]s.
    pub radius: f32,
}

impl Default for Shake {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            radius: 1.0,
        }
    }
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
    shake: Query<&Shake>,
    mut glyphs: Query<(
        &mut GlyphOffset,
        &mut ShakeOffset,
        &GlyphSpanEntity,
        &GlyphScale,
    )>,
) -> Result {
    if glyphs.is_empty() {
        return Ok(());
    }

    let mut rng = rand::rng();
    for (mut offset, mut shake_offset, span_entity, scale) in glyphs.iter_mut() {
        let shake = shake.get(span_entity.0)?;

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

    Ok(())
}
