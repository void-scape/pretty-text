use bevy::prelude::*;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{Glyph, GlyphOffset, GlyphSpanEntity};
use pretty_text::{PrettyText, PrettyTextSystems};
use pretty_text_macros::TextEffect;
use rand::Rng;

pub struct ShakePlugin;

impl Plugin for ShakePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, insert_shake)
            .add_systems(FixedUpdate, shake.before(PrettyTextSystems::GlyphPosition))
            .register_pretty_effect::<Shake>("shake");

        app.register_type::<Shake>();
    }
}

#[derive(Debug, Clone, Copy, Component, TextEffect, Reflect)]
#[require(PrettyText)]
#[pretty_text_path(pretty_text)]
pub struct Shake {
    pub intensity: f32,
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

fn insert_shake(
    mut commands: Commands,
    shake: Query<&Shake>,
    glyphs: Query<(Entity, &GlyphSpanEntity), Added<Glyph>>,
) {
    if glyphs.is_empty() {
        return;
    }

    let mut rng = rand::rng();
    for (entity, span_entity) in glyphs.iter() {
        let Ok(shake) = shake.get(span_entity.0) else {
            continue;
        };

        let end = Vec2::new(
            rng.random_range(-shake.radius..shake.radius),
            rng.random_range(-shake.radius..shake.radius),
        );
        let distance = Vec2::ZERO.distance(end);

        commands.entity(entity).insert(ShakeOffset {
            t: 0.0,
            step: if distance != 0.0 {
                shake.intensity * 2.0 / distance
            } else {
                1.0
            },
            start: Vec2::ZERO,
            end,
        });
    }
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
