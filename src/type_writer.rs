use bevy::prelude::*;

use crate::PrettyText;
use crate::glyph::{GlyphCount, GlyphOf, GlyphSystems, Glyphs, OrderedGlyphs};

pub struct TypeWriterPlugin;

impl Plugin for TypeWriterPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<GlyphRevealed>()
            .add_event::<TypeWriterFinished>()
            .add_systems(
                PostUpdate,
                (advance_reveal, reveal_glyphs.after(GlyphSystems::Construct)).chain(),
            );
    }
}

#[derive(Debug, Component)]
#[require(Reveal)]
pub struct TypeWriter(pub Timer);

impl TypeWriter {
    pub fn cps(cps: f32) -> Self {
        Self(Timer::from_seconds(1. / cps, TimerMode::Repeating))
    }
}

#[derive(Debug, Default, Component)]
#[require(PrettyText)]
pub struct Reveal(pub usize);

impl Reveal {
    pub fn all(&mut self) {
        self.0 = usize::MAX;
    }
}

#[derive(Debug, Clone, Copy, Event)]
pub struct TypeWriterFinished;

#[derive(Debug, Clone, Copy, Event)]
pub struct GlyphRevealed;

fn reveal_glyphs(
    reveal: Query<(&Glyphs, &Reveal), Or<(Changed<Reveal>, Changed<OrderedGlyphs>)>>,
    reveal_all: Query<&Glyphs>,
    mut removed: RemovedComponents<Reveal>,
    mut visibilities: Query<&mut Visibility, With<GlyphOf>>,
) {
    for (glyphs, reveal) in reveal.iter().chain(removed.read().flat_map(|entity| {
        reveal_all
            .get(entity)
            .map(|glyphs| (glyphs, &Reveal(usize::MAX)))
    })) {
        for (i, entity) in glyphs.iter().enumerate() {
            if let Ok(mut vis) = visibilities.get_mut(entity) {
                *vis = if i < reveal.0 {
                    Visibility::Visible
                } else {
                    Visibility::Hidden
                };
            }
        }
    }
}

fn advance_reveal(
    mut commands: Commands,
    time: Res<Time>,
    mut tw: Query<(Entity, &mut Reveal, &mut TypeWriter, &GlyphCount), With<Glyphs>>,
) {
    for (entity, mut reveal, mut scroll, count) in tw.iter_mut() {
        scroll.0.tick(time.delta());
        if scroll.0.just_finished() {
            if reveal.0 < count.0 {
                commands.entity(entity).trigger(GlyphRevealed);
                reveal.0 += 1;
            }

            if reveal.0 >= count.0 {
                commands
                    .entity(entity)
                    .trigger(TypeWriterFinished)
                    .remove::<(TypeWriter, Reveal)>();
            }
        }
    }
}
