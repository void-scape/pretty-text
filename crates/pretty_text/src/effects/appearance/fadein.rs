use bevy::prelude::*;
use pretty_text_macros::{DynamicEffect, parser_syntax};
use pretty_text_parser::Seconds;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSystems, mark_effect_glyphs};
use crate::glyph::{GlyphVertices, SpanGlyphOf};

use super::Appeared;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        PostUpdate,
        (mark_effect_glyphs::<FadeIn, ComputeFadeIn>, fadein)
            .chain()
            .in_set(PrettyEffectSystems),
    )
    .register_pretty_effect::<FadeIn>("fade_in");
}

/// Animates the alpha of a [`Glyph`]'s color when it [appears](Appeared).
///
/// [`Glyph`]: crate::glyph::Glyph
#[derive(Debug, Clone, Copy, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct FadeIn {
    /// Duration of the animation.
    #[syntax(default = Seconds(0.2) => "0.2", "{duration}")]
    pub duration: Seconds,

    /// Starting alpha.
    #[syntax(default = 0.0, "{number}")]
    pub start: f32,

    /// Ending alpha.
    #[syntax(default = 1.0, "{number}")]
    pub end: f32,
}

#[derive(Default, Component)]
struct ComputeFadeIn;

fn fadein(
    fades: EffectQuery<&FadeIn>,
    mut glyphs: Query<(&SpanGlyphOf, &mut GlyphVertices, &Appeared), With<ComputeFadeIn>>,
) {
    for (span_entity, mut vertices, appeared) in glyphs.iter_mut() {
        let Ok(fade) = fades.get(span_entity) else {
            continue;
        };

        if fade.duration.0 == 0.0 || appeared.0 > fade.duration.0 {
            if fade.end != 1.0 {
                vertices.color().for_each(|c| c.set_alpha(fade.end));
            }
        } else if let Some(t) = EaseFunction::SineInOut.sample(appeared.0 / fade.duration.0) {
            vertices
                .color()
                .for_each(|c| c.set_alpha(fade.start.lerp(fade.end, t)));
        }
    }
}
