use bevy::prelude::*;
use pretty_text::PrettyText;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::GlyphSpanEntity;
use pretty_text_macros::TextEffect;
use rand::Rng;

pub struct ShakePlugin;

impl Plugin for ShakePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, insert_shake)
            .add_systems(FixedUpdate, shake)
            .register_text_effect::<Shake>("shake");
    }
}

#[derive(Component, TextEffect)]
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
struct Origin(Vec2);

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
    glyphs: Query<
        (Entity, &Transform, &GlyphSpanEntity),
        (
            Or<(Changed<Visibility>, Added<Visibility>)>,
            Without<Origin>,
        ),
    >,
) {
    if glyphs.is_empty() {
        return;
    }

    let mut rng = rand::rng();
    for (entity, transform, span_entity) in glyphs.iter() {
        let Ok(shake) = shake.get(span_entity.0) else {
            continue;
        };

        let end = Vec2::new(
            rng.random_range(-shake.radius..shake.radius),
            rng.random_range(-shake.radius..shake.radius),
        );
        let distance = Vec2::ZERO.distance(end);

        commands.entity(entity).insert((
            Origin(transform.translation.xy()),
            ShakeOffset {
                t: 0.0,
                step: if distance != 0.0 {
                    shake.intensity * 2.0 / distance
                } else {
                    1.0
                },
                start: Vec2::ZERO,
                end,
            },
        ));
    }
}

fn shake(
    shake: Query<&Shake>,
    mut glyphs: Query<(&mut Transform, &Origin, &mut ShakeOffset, &GlyphSpanEntity)>,
) -> Result {
    if glyphs.is_empty() {
        return Ok(());
    }

    let mut rng = rand::rng();
    for (mut transform, origin, mut shake_offset, span_entity) in glyphs.iter_mut() {
        let shake = shake.get(span_entity.0)?;

        transform.translation = (origin.0
            + shake_offset.start.lerp(shake_offset.end, shake_offset.t))
        .extend(transform.translation.z);

        shake_offset.t += shake_offset.step;
        if shake_offset.t >= 1.0 {
            shake_offset.t = 0.0;
            shake_offset.start = transform.translation.xy() - origin.0;
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
