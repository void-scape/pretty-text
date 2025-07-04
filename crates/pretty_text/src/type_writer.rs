use std::time::Duration;

use bevy::prelude::*;

use crate::glyph::{GlyphOf, Glyphs};
use crate::{PrettyText, PrettyTextSystems};

pub struct TypeWriterPlugin;

impl Plugin for TypeWriterPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<GlyphRevealed>()
            .add_event::<TypeWriterFinished>()
            .add_event::<TypeWriterEvent>()
            .register_type::<TypeWriterEffect>()
            .add_systems(
                PostUpdate,
                (
                    type_writer,
                    reveal_glyphs.after(PrettyTextSystems::GlyphConstruct),
                )
                    .chain(),
            )
            .add_observer(removed_reveal);
    }
}

#[derive(Debug, Component)]
#[require(PrettyText, Reveal)]
pub struct TypeWriter {
    cps: f32,
    timer: Timer,
    child_index: usize,
    span_text_index: usize,
    revealed: usize,
}

impl TypeWriter {
    pub fn new(chars_per_sec: f32) -> Self {
        let dur = 1.0 / chars_per_sec;
        let mut timer = Timer::from_seconds(dur, TimerMode::Repeating);
        timer.set_elapsed(Duration::from_secs_f32(dur));

        Self {
            cps: chars_per_sec,
            timer,
            child_index: 0,
            span_text_index: 0,
            revealed: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, Event)]
pub struct TypeWriterFinished;

#[derive(Debug, Clone, Copy, Event)]
pub struct GlyphRevealed(pub Entity);

#[derive(Component)]
pub struct PauseTypeWriter(pub Timer);

impl PauseTypeWriter {
    pub fn from_seconds(duration: f32) -> Self {
        Self(Timer::from_seconds(duration, TimerMode::Once))
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

#[derive(Debug, Clone, Copy, Component, Reflect)]
pub enum TypeWriterEffect {
    Speed(f32),
    Pause(f32),
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for TypeWriterEffect {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Speed(speed) => {
                quote::quote! { ::bevy_pretty_text::type_writer::TypeWriterEffect::Speed(#speed) }
            }
            Self::Pause(duration) => {
                quote::quote! { ::bevy_pretty_text::type_writer::TypeWriterEffect::Pause(#duration) }
            }
        });
    }
}

#[derive(Debug, Clone, Component, Event)]
pub struct TypeWriterEvent(pub String);

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for TypeWriterEvent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        let event = &self.0;
        tokens.append_all(
            quote::quote! { ::bevy_pretty_text::type_writer::TypeWriterEvent(#event.into()) },
        );
    }
}

fn reveal_glyphs(
    reveal: Query<(&Glyphs, &Reveal), Or<(Changed<Reveal>, Added<Reveal>, Changed<Glyphs>)>>,
    mut visibilities: Query<&mut Visibility, With<GlyphOf>>,
) {
    for (glyphs, reveal) in reveal.iter() {
        for (i, entity) in glyphs.iter().enumerate() {
            if let Ok(mut vis) = visibilities.get_mut(entity) {
                let target = if i < reveal.0 {
                    Visibility::Inherited
                } else {
                    Visibility::Hidden
                };

                if *vis != target {
                    *vis = target;
                }
            }
        }
    }
}

fn removed_reveal(
    trigger: Trigger<OnRemove, Reveal>,
    mut visibilities: Query<&mut Visibility, With<GlyphOf>>,
    removed: Query<&Glyphs>,
) {
    if let Ok(glyphs) = removed.get(trigger.target()) {
        for entity in glyphs.iter() {
            if let Ok(mut vis) = visibilities.get_mut(entity) {
                if *vis != Visibility::Inherited {
                    *vis = Visibility::Inherited;
                }
            }
        }
    }
}

pub fn type_writer(
    mut commands: Commands,
    time: Res<Time>,
    mut type_wrtiers: Query<(
        Entity,
        &mut TypeWriter,
        &mut Reveal,
        Option<&mut PauseTypeWriter>,
        &Glyphs,
        &Children,
    )>,
    mut writer: EventWriter<TypeWriterEvent>,
    effects: Query<&TypeWriterEffect>,
    spans: Query<&TextSpan>,
    events: Query<&TypeWriterEvent>,
) {
    for (entity, mut tw, mut reveal, pause, glyphs, children) in type_wrtiers.iter_mut() {
        if let Some(mut pause) = pause {
            pause.0.tick(time.delta());
            if pause.0.finished() {
                commands.entity(entity).remove::<PauseTypeWriter>();
                tw.child_index += 1;
            }
            continue;
        }

        if tw.child_index >= children.len() {
            commands
                .entity(entity)
                .remove::<(TypeWriter, Reveal)>()
                .trigger(TypeWriterFinished);
            continue;
        }

        let child = children[tw.child_index];

        if let Ok(effect) = effects.get(child) {
            match *effect {
                TypeWriterEffect::Pause(dur) => {
                    commands
                        .entity(entity)
                        .insert(PauseTypeWriter::from_seconds(dur));
                }
                TypeWriterEffect::Speed(speed) => {
                    let cps = tw.cps;
                    tw.timer
                        .set_duration(Duration::from_secs_f32(1. / cps / speed));
                    tw.child_index += 1;
                }
            }
        } else if let Ok(span) = spans.get(child) {
            tw.timer.tick(time.delta());
            if tw.timer.just_finished() {
                if tw.span_text_index < span.0.len() {
                    let glyph_entity = glyphs.collection()[tw.revealed];

                    tw.span_text_index += 1;
                    tw.revealed += 1;
                    reveal.0 = tw.revealed;

                    commands.entity(entity).trigger(GlyphRevealed(glyph_entity));
                }

                if tw.span_text_index >= span.0.len() {
                    tw.child_index += 1;
                    tw.span_text_index = 0;
                }
            }
        } else if let Ok(event) = events.get(child) {
            writer.write(event.clone());
            commands.entity(entity).trigger(event.clone());
            tw.child_index += 1;
        } else {
            tw.child_index += 1;
        }
    }
}
