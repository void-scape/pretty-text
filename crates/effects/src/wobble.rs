use bevy::prelude::*;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{Glyph, GlyphOffset, GlyphSpanEntity};
use pretty_text::{PrettyText, PrettyTextSystems};
use pretty_text_macros::TextEffect;

pub struct WobblePlugin;

impl Plugin for WobblePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, insert_wobble)
            .add_systems(FixedUpdate, wobble.before(PrettyTextSystems::GlyphPosition))
            .register_pretty_effect::<Wobble>("wobble");
    }
}

#[derive(Component, TextEffect)]
#[require(PrettyText)]
#[pretty_text_path(pretty_text)]
pub struct Wobble {
    pub intensity: f32,
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

#[derive(Component)]
struct ComputeWobble;

fn insert_wobble(
    mut commands: Commands,
    wobbles: Query<&Wobble>,
    glyphs: Query<(Entity, &GlyphSpanEntity), Added<Glyph>>,
) {
    for (entity, span_entity) in glyphs.iter() {
        if wobbles.get(span_entity.0).is_err() {
            continue;
        };
        commands.entity(entity).insert(ComputeWobble);
    }
}

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
