use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphSystems;
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphOrigin, GlyphSpanEntity};
use pretty_text_macros::TextEffect;

use crate::apply_effect_on_glyphs;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(FixedUpdate, wave.before(GlyphSystems::Position))
        .register_pretty_effect::<Wave>("wave")
        .add_observer(apply_effect_on_glyphs::<Wave, ComputeWave>);

    app.register_type::<Wave>();
}

/// Applies oscillating motion to a glyph along the y-axis.
///
/// ```
#[doc = include_str!("docs/header")]
/// // Parsed usage
/// world.spawn(pretty!("`my text`[wave(1, 20)]"));
/// world.spawn(PrettyTextParser::bundle("`my text`[wave(1, 20)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text2d::new("my text"),
///     Wave {
///         intensity: 1.0,
///         max_height: 20.0,
///     },
/// ));
#[doc = include_str!("docs/footer")]
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect, TextEffect)]
#[require(PrettyText)]
pub struct Wave {
    /// Controls the speed of movement.
    pub intensity: f32,

    /// Maximum [`Transform::translation`] displacement along the y-axis from the glyph origin.
    pub max_height: f32,
}

impl Default for Wave {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            max_height: 20.0,
        }
    }
}

/// Marks glyph as target for the [`Wave`] effect.
#[derive(Default, Component)]
pub(super) struct ComputeWave;

fn wave(
    time: Res<Time>,
    waves: Query<&Wave>,
    mut glyphs: Query<(&mut GlyphOffset, &GlyphOrigin, &GlyphSpanEntity), With<ComputeWave>>,
) -> Result {
    for (mut offset, origin, span_entity) in glyphs.iter_mut() {
        let wave = waves.get(span_entity.0)?;
        let time_factor = time.elapsed_secs() * wave.intensity;
        let wave_value = (origin.x * 0.02 + time_factor * 10.0).sin() * 0.4;
        offset.0.y += wave_value * wave.max_height * time.delta_secs() * 60.0;
    }

    Ok(())
}
