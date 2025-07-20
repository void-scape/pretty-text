use bevy::prelude::*;
use bevy_pretty_text::glyph::{GlyphScale, GlyphSystems};
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphOrigin, GlyphSpanEntity};
use pretty_text_macros::DynamicEffect;

use crate::apply_effect_on_glyphs;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Update, wave.before(GlyphSystems::Position))
        .register_pretty_effect::<Wave>("wave")
        .add_observer(apply_effect_on_glyphs::<Wave, ComputeWave>);

    app.register_type::<Wave>();
}

/// Applies oscillating motion to a glyph along the y-axis.
///
/// ```
#[doc = include_str!("docs/header")]
/// // Parsed usage
/// world.spawn(pretty!("`my text`[wave(1, 1)]"));
/// world.spawn(PrettyTextParser::bundle("`my text`[wave(1, 1)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text2d::new("my text"),
///     Wave {
///         intensity: 1.0,
///         max_height: 1.0,
///     },
/// ));
#[doc = include_str!("docs/footer")]
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
pub struct Wave {
    /// Controls the speed of movement.
    pub intensity: f64,

    /// Maximum displacement along the y-axis from the glyph origin.
    ///
    /// The `max_height` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`Transform::scale`]s.
    pub max_height: f32,
}

impl Default for Wave {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            max_height: 1.0,
        }
    }
}

/// Marks glyph as target for the [`Wave`] effect.
#[derive(Default, Component)]
pub struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: Query<&Wave>,
    mut glyphs: Query<
        (
            &mut GlyphOffset,
            &GlyphOrigin,
            &GlyphSpanEntity,
            &GlyphScale,
        ),
        With<ComputeWave>,
    >,
) -> Result {
    for (mut offset, origin, span_entity, scale) in glyphs.iter_mut() {
        let wave = waves.get(span_entity.0)?;
        let time_factor = time.elapsed_secs_f64() * wave.intensity;
        let wave_value = (-origin.x as f64 * 0.02 + time_factor * 10.0).sin() * 0.4;
        offset.0.y += wave_value as f32 * wave.max_height * scale.y * 6f32;
    }

    Ok(())
}
