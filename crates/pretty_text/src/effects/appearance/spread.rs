use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::effects::EffectQuery;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::tween::*;
use crate::glyph::{Glyph, GlyphSpan, LocalGlyphScale};
use crate::{PrettyText, animation, lens};

use super::Appeared;

pub(super) fn plugin(app: &mut App) {
    app.add_observer(spread)
        .register_pretty_effect::<Spread>("spread");

    app.register_type::<Spread>();
}

/// Overshoots glyph y-scale and rebounds, bouncing.
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Spread {
    /// Rate that the scale oscillates.
    #[syntax(default = 1.0, "{number}")]
    pub frequency: f32,

    /// Minimum scale.
    #[syntax(default = 0.0, "{number}")]
    pub min: f32,

    /// Maximum scale.
    #[syntax(default = 1.4, "{number}")]
    pub max: f32,
}

fn spread(
    trigger: Trigger<OnInsert, Appeared>,
    mut commands: Commands,
    spread: EffectQuery<&Spread>,
    glyphs: Query<&GlyphSpan, With<Glyph>>,
) {
    let Ok(span_entity) = glyphs.get(trigger.target()) else {
        return;
    };

    let Ok(spread) = spread.get(span_entity) else {
        return;
    };

    let min = Vec2::new(0.0, spread.min - 1.0);
    let max = Vec2::new(0.0, spread.max - 1.0);

    commands.spawn((
        TimeRunner,
        ChildOf(trigger.target()),
        AnimationTarget(trigger.target()),
        LensMode::Accumulate,
        lens!(LocalGlyphScale::0),
        animation![
            (
                AnimationCurve(EaseFunction::SineOut),
                AnimationDuration::from_secs(0.1),
                InitialValue(min),
                KeyFrame(max),
            ),
            (
                AnimationCurve(EaseFunction::BounceOut),
                AnimationDuration::from_secs(0.6),
                InitialValue(max),
                KeyFrame(Vec2::ZERO),
            ),
        ],
    ));
}
