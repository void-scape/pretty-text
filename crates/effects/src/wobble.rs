use bevy::prelude::*;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphSpanEntity};
use pretty_text::{PrettyText, PrettyTextSystems};
use pretty_text_macros::TextEffect;

use crate::apply_effect_on_glyphs;

pub(super) struct WobblePlugin;

impl Plugin for WobblePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(FixedUpdate, wobble.before(PrettyTextSystems::GlyphPosition))
            .register_pretty_effect::<Wobble>("wobble")
            .add_observer(apply_effect_on_glyphs::<Wobble, ComputeWobble>);

        app.register_type::<Wobble>();
    }
}

/// Applies complex circular motion to a glyph along both x and y axes.
///
/// See [`bevy_pretty_text::parser`].
///
/// ```
#[doc = include_str!("docs/header")]
/// // Parsed usage
/// world.spawn(pretty!("`my text`[wobble(1.0, 5.0)]"));
/// world.spawn(PrettyTextParser::parse("`my text`[wobble(1.0, 5.0)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text2d::new("my text"),
///     Wobble {
///         intensity: 1.0,
///         radius: 5.0,
///     },
/// ));
#[doc = include_str!("docs/footer")]
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect, TextEffect)]
#[require(PrettyText)]
pub struct Wobble {
    /// Controls the speed of movement.
    pub intensity: f32,

    /// Maximum [`Transform::translation`] displacement from the glyph origin.
    pub radius: f32,
}

impl Default for Wobble {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            radius: 5.0,
        }
    }
}

/// Marks glyph as target for the [`Wobble`] effect.
#[derive(Default, Component)]
pub(super) struct ComputeWobble;

fn wobble(
    time: Res<Time>,
    wobbles: Query<&Wobble>,
    mut glyphs: Query<(&mut GlyphOffset, &GlyphSpanEntity), With<ComputeWobble>>,
) -> Result {
    for (i, (mut offset, span_entity)) in glyphs.iter_mut().enumerate() {
        let wobble = wobbles.get(span_entity.0)?;
        let i = i as f32;
        let time_factor = time.elapsed_secs() * wobble.intensity * 8.0;
        let x = time_factor.sin() * (time_factor * 1.3 + i * 2.0).cos();
        let y = time_factor.cos() * (time_factor * 1.7 + i * 3.0).sin();
        offset.0 += (Vec2::new(x, y) * wobble.radius * time.delta_secs() * 60.0).extend(0.);
    }

    Ok(())
}
