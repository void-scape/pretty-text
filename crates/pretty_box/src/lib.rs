use bevy::prelude::*;
use bevy_pretty_text::{access::GlyphReader, prelude::*};
use bevy_seedling::prelude::*;
use bevy_sequence::{
    fragment::{DataLeaf, event::InsertBeginDown},
    prelude::*,
};
use std::{marker::PhantomData, time::Duration};

pub struct PrettyBoxPlugin;

impl Plugin for PrettyBoxPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(Character {
            name: None,
            text_sound: "talk-low.wav",
        })
        .add_plugins((
            bevy_pretty_text::PrettyTextPlugin,
            bevy_sequence::SequencePlugin,
            bevy_seedling::SeedlingPlugin::default(),
        ))
        .add_event::<FragmentEvent<AudioSequence>>()
        .add_systems(
            Update,
            (
                textbox_handler,
                tick_pauses,
                sequence_runner,
                update_name,
                ApplyDeferred,
            )
                .chain(),
        )
        .add_observer(glyph_reveal);
    }
}

fn glyph_reveal(
    trigger: Trigger<GlyphRevealed>,
    mut commands: Commands,
    server: Res<AssetServer>,
    character: Res<Character>,
    reader: GlyphReader,
) -> Result {
    let char = reader.read(trigger.0)?;
    if char == " " {
        return Ok(());
    }
    commands.spawn(SamplePlayer::new(server.load(character.text_sound)));
    Ok(())
}

#[derive(Clone)]
pub enum AudioSequence {
    Pause(Duration),
    Text(String),
}

impl IntoFragment<AudioSequence> for &'static str {
    fn into_fragment(self, context: &Context<()>, commands: &mut Commands) -> FragmentId {
        let leaf = DataLeaf::new(AudioSequence::Text(self.into()));

        <_ as IntoFragment<AudioSequence>>::into_fragment(leaf, context, commands)
    }
}

impl IntoFragment<AudioSequence> for f32 {
    fn into_fragment(self, context: &Context<()>, commands: &mut Commands) -> FragmentId {
        let leaf = DataLeaf::new(AudioSequence::Pause(Duration::from_secs_f32(self)));

        <_ as IntoFragment<AudioSequence>>::into_fragment(leaf, context, commands)
    }
}

pub struct DynamicText<S, O, M> {
    system: S,
    marker: PhantomData<fn() -> (O, M)>,
}

pub fn dynamic<S, O, M>(system: S) -> DynamicText<S, O, M>
where
    S: IntoSystem<(), O, M>,
{
    DynamicText {
        system,
        marker: PhantomData,
    }
}

impl<S, O, M> IntoFragment<AudioSequence> for DynamicText<S, O, M>
where
    S: IntoSystem<(), String, M> + Send + 'static,
{
    fn into_fragment(self, _: &Context<()>, commands: &mut Commands) -> FragmentId {
        let system = commands.register_system(self.system);
        let id = commands
            .spawn(bevy_sequence::fragment::Leaf)
            .insert_begin_down(move |event, world| {
                let string = world.run_system(system)?;

                world.send_event(FragmentEvent {
                    id: event.id,
                    data: AudioSequence::Text(string),
                });

                Ok(())
            })
            .id();

        FragmentId::new(id)
    }
}

#[derive(Component)]
#[require(Name::new("Textbox"))]
pub struct Textbox(FragmentEndEvent);

#[derive(Component)]
#[require(Name::new("Textbox Container"))]
pub struct TextboxContainer;

#[derive(Component)]
#[require(Name::new("Textbox Name"))]
pub struct TextboxName;

#[derive(Component)]
#[require(Name::new("Textbox Continue"))]
pub struct TextboxContinue;

fn textbox_handler(
    mut commands: Commands,
    server: Res<AssetServer>,
    textbox: Single<(Entity, &Textbox, Has<TypeWriter>)>,
    tcontinue: Query<Entity, With<TextboxContinue>>,
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
            .remove::<(TypeWriter, Reveal)>()
            .trigger(TypeWriterFinished);
    } else {
        end_events.write(textbox.0);
        commands.entity(entity).despawn();

        if let Ok(triangle) = tcontinue.single() {
            commands.entity(triangle).despawn();
        }

        commands.spawn(SamplePlayer::new(server.load("click.ogg")));
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

#[derive(Resource)]
pub struct Character {
    pub name: Option<&'static str>,
    pub text_sound: &'static str,
}

pub trait CharacterFragment
where
    Self: Sized + IntoFragment<AudioSequence>,
{
    fn narrator(self) -> impl IntoFragment<AudioSequence> {
        self.on_start(|mut character: ResMut<Character>| {
            character.name = None;
            character.text_sound = "talk-low.wav";
        })
    }

    fn stranger(self) -> impl IntoFragment<AudioSequence> {
        self.on_start(|mut character: ResMut<Character>| {
            character.name = Some("Stranger");
            character.text_sound = "talk.wav";
        })
    }

    fn aster(self) -> impl IntoFragment<AudioSequence> {
        self.on_start(|mut character: ResMut<Character>| {
            character.name = Some("Aster");
            character.text_sound = "talk.wav";
        })
    }
}

impl<T> CharacterFragment for T where T: IntoFragment<AudioSequence> {}

fn update_name(
    mut names: Query<(Entity, Option<&mut Text2d>), With<TextboxName>>,
    character: Res<Character>,
    mut commands: Commands,
) {
    for (name, text) in &mut names {
        match (character.name, text) {
            (Some(name_string), Some(mut text)) => {
                text.clear();
                *text = name_string.into();
            }
            (Some(name_string), None) => {
                commands.entity(name).insert(Text2d::new(name_string));
            }
            (None, _) => {
                commands.entity(name).remove::<Text2d>();
            }
        }
    }
}

pub fn despawn_textbox(container: Query<Entity, With<TextboxContainer>>, mut commands: Commands) {
    if let Ok(container) = container.single() {
        commands.entity(container).despawn();
    }
}

fn sequence_runner(
    mut start_events: EventReader<FragmentEvent<AudioSequence>>,
    container: Query<Entity, With<TextboxContainer>>,
    mut commands: Commands,
) -> Result {
    for event in start_events.read() {
        match &event.data {
            AudioSequence::Pause(pause) => {
                commands.spawn(SequencePause {
                    timer: Timer::new(*pause, TimerMode::Once),
                    event: event.end(),
                });

                if let Ok(container) = container.single() {
                    commands.entity(container).despawn();
                }
            }
            AudioSequence::Text(text) => {
                let container = container.single().unwrap_or_else(|_| {
                    let container = commands
                        .spawn((TextboxContainer, Visibility::Visible, Transform::default()))
                        .id();
                    commands.spawn((ChildOf(container), TextboxName));
                    container
                });

                commands
                    .spawn((
                        Textbox(event.end()),
                        PrettyTextParser::parse(text)?,
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
