use bevy::prelude::*;
use bevy_pretty_text::glyph::{GlyphScale, GlyphSystems};
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphSpanEntity};
use pretty_text_macros::DynamicGlyphEffect;

use crate::apply_effect_on_glyphs;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Update, wobble.before(GlyphSystems::Position))
        .register_pretty_effect::<Wobble>("wobble")
        .add_observer(apply_effect_on_glyphs::<Wobble, ComputeWobble>);

    app.register_type::<Wobble>();
}

/// Applies complex circular motion to a glyph along both x and y axes.
///
/// ```
#[doc = include_str!("../docs/header.txt")]
/// // Parsed usage
/// world.spawn(pretty!("`my text`[wobble(1.0, 1.0)]"));
/// world.spawn(PrettyParser::bundle("`my text`[wobble(1.0, 1.0)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text::new("my text"),
///     Wobble {
///         intensity: 1.0,
///         radius: 1.0,
///     },
/// ));
#[doc = include_str!("../docs/footer.txt")]
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicGlyphEffect)]
#[require(PrettyText)]
pub struct Wobble {
    /// Controls the speed of movement.
    ///
    /// The `intensity` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`Transform::scale`]s.
    pub intensity: f64,

    /// Maximum displacement from the glyph origin.
    ///
    /// The `radius` is scaled uniformly across different [`TextFont::font_size`]s
    /// and [`Transform::scale`]s.
    pub radius: f32,
}

impl Default for Wobble {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            radius: 1.0,
        }
    }
}

/// Marks glyph as target for the [`Wobble`] effect.
#[derive(Default, Component)]
pub struct ComputeWobble;

fn wobble(
    time: Res<Time>,
    wobbles: Query<&Wobble>,
    mut glyphs: Query<(&mut GlyphOffset, &GlyphSpanEntity, &GlyphScale), With<ComputeWobble>>,
) -> Result {
    for (i, (mut offset, span_entity, scale)) in glyphs.iter_mut().enumerate() {
        let wobble = wobbles.get(span_entity.0)?;
        let i = i as f64;
        let time_factor = time.elapsed_secs_f64() * wobble.intensity * 8.0;
        let x = time_factor.sin() * (time_factor * 1.3 + i * 2.0).cos();
        let y = time_factor.cos() * (time_factor * 1.7 + i * 3.0).sin();
        offset.0 += (Vec2::new(x as f32, y as f32) * wobble.radius * scale.0).extend(0.);
    }

    Ok(())
}
