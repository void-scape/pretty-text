use bevy::prelude::*;
use bevy_pretty_text::glyph::GlyphScale;
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{GlyphOffset, GlyphSpanEntity};
use pretty_text_macros::{DynamicEffect, dynamic_effect_docs};

use crate::{PrettyEffectSet, apply_effect_on_glyphs};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Update, wobble.in_set(PrettyEffectSet))
        .register_pretty_effect::<Wobble>("wobble")
        .add_observer(apply_effect_on_glyphs::<Wobble, ComputeWobble>);

    app.register_type::<Wobble>();
}

/// Applies complex circular motion to a glyph along both x and y axes.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[dynamic_effect_docs]
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
