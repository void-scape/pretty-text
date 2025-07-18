use bevy::prelude::*;
use bevy_pretty_text::parser::PrettyTextSpans;
use bevy_pretty_text::prelude::*;
use bevy_pretty_text::type_writer::Reveal;
use bevy_sequence::{fragment::DataLeaf, prelude::*};
use std::time::Duration;

pub struct PrettyBoxPlugin;

impl Plugin for PrettyBoxPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<TextboxAdvance>()
            .add_plugins((
                bevy_pretty_text::PrettyTextPlugin,
                bevy_sequence::SequencePlugin,
            ))
            .add_event::<FragmentEvent<PrettySequence>>()
            .add_systems(
                Update,
                (textbox_handler, tick_pauses, sequence_runner).chain(),
            );

        app.register_type::<TextboxContainer>()
            .register_type::<TextboxName>()
            .register_type::<TextboxContinue>()
            .register_type::<TextboxAdvance>();
    }
}

#[derive(Debug, Clone, Copy, Component)]
#[require(Name::new("Textbox"), TextboxName)]
pub struct Textbox(FragmentEndEvent);

#[derive(Debug, Default, Clone, Component, Reflect)]
pub struct TextboxName(pub Option<String>);

impl TextboxName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(Some(name.into()))
    }
}

#[derive(Debug, Default, Clone, Copy, Component, Reflect)]
#[require(Name::new("Textbox Container"))]
pub struct TextboxContainer;

#[derive(Debug, Default, Clone, Copy, Component, Reflect)]
#[require(Name::new("Textbox Continue"))]
pub struct TextboxContinue;

#[derive(Debug, Default, Clone, Copy, Event, Reflect)]
pub struct TextboxAdvance;

fn textbox_handler(
    mut commands: Commands,
    container: Single<Entity, With<TextboxContainer>>,
    textbox: Single<(Entity, &Textbox, Has<TypeWriter>)>,
    tcontinue: Option<Single<Entity, With<TextboxContinue>>>,
    keys: Res<ButtonInput<KeyCode>>,
    mut end_events: EventWriter<FragmentEndEvent>,
) {
    if !keys.just_pressed(KeyCode::Space) {
        return;
    }

    let (entity, textbox, has_typewriter) = textbox.into_inner();

    if has_typewriter {
        commands
            .entity(entity)
            .remove::<(TypeWriter, Reveal, TypeWriterMode)>()
            .trigger(TypeWriterFinished);
    } else {
        end_events.write(textbox.0);
        commands.entity(entity).despawn();
        if let Some(tcontinue) = tcontinue {
            commands.entity(*tcontinue).despawn();
        }
        commands.entity(*container).trigger(TextboxAdvance);
    }
}

#[derive(Component)]
struct SequencePause {
    timer: Timer,
    event: FragmentEndEvent,
}

fn tick_pauses(
    mut pauses: Query<(Entity, &mut SequencePause)>,
    mut end_events: EventWriter<FragmentEndEvent>,
    time: Res<Time>,
    mut commands: Commands,
) {
    let delta = time.delta();
    for (entity, mut pause) in &mut pauses {
        if pause.timer.tick(delta).just_finished() {
            commands.entity(entity).despawn();
            end_events.write(pause.event);
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrettySequence {
    Pause(Duration),
    Text(String),
    StaticText(PrettyTextSpans),
}

impl IntoFragment<PrettySequence> for PrettyTextSpans {
    fn into_fragment(self, context: &Context<()>, commands: &mut Commands) -> FragmentId {
        let leaf = DataLeaf::new(PrettySequence::StaticText(self));

        <_ as IntoFragment<PrettySequence>>::into_fragment(leaf, context, commands)
    }
}

impl IntoFragment<PrettySequence> for &'static str {
    fn into_fragment(self, context: &Context<()>, commands: &mut Commands) -> FragmentId {
        let leaf = DataLeaf::new(PrettySequence::Text(self.into()));

        <_ as IntoFragment<PrettySequence>>::into_fragment(leaf, context, commands)
    }
}

impl IntoFragment<PrettySequence> for f32 {
    fn into_fragment(self, context: &Context<()>, commands: &mut Commands) -> FragmentId {
        let leaf = DataLeaf::new(PrettySequence::Pause(Duration::from_secs_f32(self)));

        <_ as IntoFragment<PrettySequence>>::into_fragment(leaf, context, commands)
    }
}

pub fn despawn_textbox(
    container: Option<Single<Entity, With<TextboxContainer>>>,
    mut commands: Commands,
) {
    if let Some(textbox) = container {
        commands.entity(*textbox).despawn();
    }
}

fn sequence_runner(
    mut start_events: EventReader<FragmentEvent<PrettySequence>>,
    container: Option<Single<Entity, With<TextboxContainer>>>,
    mut commands: Commands,
) -> Result {
    for event in start_events.read() {
        match &event.data {
            PrettySequence::Pause(pause) => {
                commands.spawn(SequencePause {
                    timer: Timer::new(*pause, TimerMode::Once),
                    event: event.end(),
                });

                if let Some(container) = &container {
                    commands.entity(**container).despawn();
                }
            }
            PrettySequence::StaticText(bundle) => {
                let container = container
                    .as_ref()
                    .map(|container| **container)
                    .unwrap_or_else(|| {
                        commands
                            .spawn((TextboxContainer, Visibility::Visible, Transform::default()))
                            .id()
                    });

                commands
                    .spawn((
                        Textbox(event.end()),
                        bundle.clone().into_bundle(),
                        ChildOf(container),
                    ))
                    .observe(
                        move |_: Trigger<TypeWriterFinished>, mut commands: Commands| {
                            commands.entity(container).with_child(TextboxContinue);
                        },
                    );
            }
            PrettySequence::Text(text) => {
                let container = container
                    .as_ref()
                    .map(|container| **container)
                    .unwrap_or_else(|| {
                        commands
                            .spawn((TextboxContainer, Visibility::Visible, Transform::default()))
                            .id()
                    });

                commands
                    .spawn((
                        Textbox(event.end()),
                        PrettyTextParser::bundle(text)?,
                        ChildOf(container),
                    ))
                    .observe(
                        move |_: Trigger<TypeWriterFinished>, mut commands: Commands| {
                            commands.entity(container).with_child(TextboxContinue);
                        },
                    );
            }
        }
    }

    Ok(())
}
