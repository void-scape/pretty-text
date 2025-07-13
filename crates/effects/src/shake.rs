use bevy::prelude::*;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{Glyph, GlyphOffset, GlyphSpanEntity};
use pretty_text::{PrettyText, PrettyTextSystems};
use pretty_text_macros::TextEffect;
use rand::Rng;

pub(super) struct ShakePlugin;

impl Plugin for ShakePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(FixedUpdate, shake.before(PrettyTextSystems::GlyphPosition))
            .register_pretty_effect::<Shake>("shake")
            .add_observer(apply_shake);

        app.register_type::<Shake>();
    }
}

/// Applies random linear motion within a radius.
///
/// See [`bevy_pretty_text::parser`].
///
/// ```
#[doc = include_str!("docs/header")]
/// // Parsed usage
/// world.spawn(pretty!("`my text`[shake(1, 5)]"));
/// world.spawn(PrettyTextParser::parse("`my text`[shake(1, 5)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text2d::new("my text"),
///     Shake {
///         intensity: 1.0,
///         radius: 5.0,
///     },
/// ));
#[doc = include_str!("docs/footer")]
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect, TextEffect)]
#[require(PrettyText)]
pub struct Shake {
    /// Controls the speed of movement.
    pub intensity: f32,

    /// Maximum [`Transform::translation`] displacement from the glyph origin.
    pub radius: f32,
}

impl Default for Shake {
    fn default() -> Self {
        Self {
            intensity: 1.0,
            radius: 5.0,
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

fn apply_shake(
    trigger: Trigger<OnAdd, Glyph>,
    mut commands: Commands,
    spans: Query<&GlyphSpanEntity>,
    effects: Query<&Shake>,
) -> Result {
    let span_entity = spans.get(trigger.target())?;
    let Ok(shake) = effects.get(span_entity.0) else {
        return Ok(());
    };

    let mut rng = rand::rng();

    let end = Vec2::new(
        rng.random_range(-shake.radius..shake.radius),
        rng.random_range(-shake.radius..shake.radius),
    );
    let distance = Vec2::ZERO.distance(end);

    commands.entity(trigger.target()).insert(ShakeOffset {
        t: 0.0,
        step: if distance != 0.0 {
            shake.intensity * 2.0 / distance
        } else {
            1.0
        },
        start: Vec2::ZERO,
        end,
    });

    Ok(())
}

fn shake(
    time: Res<Time>,
    shake: Query<&Shake>,
    mut glyphs: Query<(&mut GlyphOffset, &mut ShakeOffset, &GlyphSpanEntity)>,
) -> Result {
    if glyphs.is_empty() {
        return Ok(());
    }

    let mut rng = rand::rng();
    for (mut offset, mut shake_offset, span_entity) in glyphs.iter_mut() {
        let shake = shake.get(span_entity.0)?;

        let new_offset = shake_offset.start.lerp(shake_offset.end, shake_offset.t);
        offset.0 += (new_offset * time.delta_secs() * 60.0).extend(0.);

        shake_offset.t += shake_offset.step;
        if shake_offset.t >= 1.0 {
            shake_offset.t = 0.0;
            shake_offset.start = new_offset;
            shake_offset.end = Vec2::new(
                rng.random_range(-shake.radius..shake.radius),
                rng.random_range(-shake.radius..shake.radius),
            );

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
