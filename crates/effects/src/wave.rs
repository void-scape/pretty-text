use bevy::prelude::*;
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{Glyph, GlyphOffset, GlyphOrigin, GlyphSpanEntity};
use pretty_text::{PrettyText, PrettyTextSystems};
use pretty_text_macros::TextEffect;

pub struct WavePlugin;

impl Plugin for WavePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, insert_wave)
            .add_systems(FixedUpdate, wave.before(PrettyTextSystems::GlyphPosition))
            .register_pretty_effect::<Wave>("wave");
    }
}

#[derive(Component, TextEffect)]
#[require(PrettyText)]
#[pretty_text_path(pretty_text)]
pub struct Wave {
    pub intensity: f32,
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

#[derive(Component)]
struct ComputeWave;

fn insert_wave(
    mut commands: Commands,
    waves: Query<&Wave>,
    glyphs: Query<(Entity, &GlyphSpanEntity), Added<Glyph>>,
) {
    for (entity, span_entity) in glyphs.iter() {
        if waves.get(span_entity.0).is_err() {
            continue;
        };
        commands.entity(entity).insert(ComputeWave);
    }
}

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
