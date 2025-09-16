//! Provides the beloved **type writer** text effect with additional
//! sequencing including:
//! - Changing speed
//! - Pausing
//! - Emitting events
//! - Running one shot systems
//!
//! For more detail, see [`Typewriter`].

use std::collections::VecDeque;
use std::time::Duration;

use bevy::prelude::*;

use crate::PrettyText;
use crate::effects::appearance::Appeared;
// NOTE: import for documentation
#[allow(unused)]
use crate::glyph::Glyph;
use crate::glyph::{GlyphReader, GlyphSystems, Glyphs, SpanGlyphs, Words};

use hierarchy::{TypewriterCallback, TypewriterCommand, TypewriterEvent};

pub mod hierarchy;

/// A [`SystemSet`] for [`Typewriter`] systems.
///
/// Runs in the [`Update`] schedule.
#[derive(Debug, Clone, Copy, SystemSet, Eq, PartialEq, Hash)]
pub struct TypewriterSet;

/// A plugin for managing [`Typewriter`] entities.
#[derive(Debug)]
pub struct TypewriterPlugin;

impl Plugin for TypewriterPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<GlyphRevealed>()
            .add_event::<TypewriterFinished>()
            .add_event::<TypewriterEvent>()
            .register_type::<TypewriterCommand>()
            .add_systems(
                Update,
                (
                    step,
                    start_sequence,
                    commands,
                    (events, callbacks),
                    pause,
                    end_sequence,
                    finish,
                )
                    .chain()
                    .in_set(TypewriterSet),
            )
            .add_systems(PostUpdate, initialize.after(GlyphSystems::Construct))
            .add_observer(reveal_glyph);

        app.register_type::<Typewriter>()
            .register_type::<TypewriterIndex>()
            .register_type::<TypewriterFinished>()
            .register_type::<GlyphRevealed>()
            .register_type::<PauseTypewriter>()
            .register_type::<TypewriterCommand>()
            .register_type::<TypewriterEvent>();
    }
}

/// [`Typewriter`] reveals text over time.
///
/// Placing [`Typewriter`] into a [`Text`] or [`Text2d`] entity will immediately hide and
/// begin revealing text.
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     Typewriter::new(30.0),
///     Text::new("my text"),
/// ));
///
/// // Reveal by words.
/// world.spawn((
///     Typewriter::new(2.0),
///     TypewriterIndex::word(),
///     Text::new("my text"),
/// ));
/// ```
///
/// # Special Sequencing
///
/// The [`Typewriter`] has special syntax for creating sequencing effects. These
/// effects include:
/// - Changing speed with [`TypewriterCommand::Speed`].
/// - Pausing with [`TypewriterCommand::Pause`].
/// - Emitting events with [`TypewriterEvent`].
/// - Running one shot systems with [`TypewriterCallback`].
///
/// For the special type writer syntax, see [`parser`](crate::parser#type-writer-syntax).
///
/// To understand how the effects are represented in the ECS, see [`hierarchy`].
///
/// # Revealing Text
///
/// [`Typewriter`]s can reveal either glyphs (the default) or words, configurable with [`TypewriterIndex`].
/// The [`Typewriter`] entity will trigger events related to the revealed text:
/// - [`GlyphRevealed`]
/// - [`WordRevealed`]
///
/// These events will fire regardless of the [`TypewriterIndex`] mode.
///
/// You can observe these events to, for example, play a sound:
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
#[doc = include_str!("../../docs/audio_player.txt")]
/// # let mut world = World::new();
/// world
///     .spawn((
///         Typewriter::new(30.0),
///         Text::new("my text"),
///     ))
///     .observe(
///         |_: Trigger<GlyphRevealed>, mut commands: Commands, server: Res<AssetServer>| {
///             commands.spawn(AudioPlayer::new(
///                 server.load("revealed-glyph.ogg"),
///             ));
///         },
///     );
/// ```
///
/// # Completion
///
/// Once a [`Typewriter`] has revealed the entire text hierarchy, the entity will remove its
/// [`Typewriter`] related components and trigger the [`TypewriterFinished`] event.
///
/// # Early Completion
///
/// In some cases it is useful to advance the [`Typewriter`] to the end of the sequence.
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// fn finish(mut commands: Commands, typewriter: Single<Entity, With<Typewriter>>) {
///     // Finishing the typewriter will reveal remaining gylphs *and* trigger
///     // remaining events and callbacks.
///     commands.entity(*typewriter).insert(FinishTypewriter);
/// }
///
/// fn short_circuit(mut commands: Commands, typewriter: Single<Entity, With<Typewriter>>) {
///     // Short circuiting the typewriter will reveal remaining glyphs but *skip*
///     // remaining events and callbacks.
///     commands.entity(*typewriter).insert(ShortCircuitTypewriter);
/// }
/// ```
///
/// In both cases, a [`TypewriterFinished`] event will be triggered.
#[derive(Debug, Clone, Component, Reflect)]
#[require(PrettyText, TypewriterIndex, Initialize)]
pub struct Typewriter {
    speed: f32,
    timer: Timer,
    completed_sequences: usize,
    queued_sequences: VecDeque<Entity>,
    process: usize,
    finished_glyphs: bool,
}

impl Typewriter {
    /// Creates a new `Typewriter` with `speed`.
    ///
    /// The unit of `speed` is:
    /// - glyphs/second (when configured with [`TypewriterIndex::Glyph`])
    /// - words/second (when configured with [`TypewriterIndex::Word`])
    pub fn new(speed: f32) -> Self {
        Self {
            speed,
            timer: Self::new_timer(speed),
            completed_sequences: 0,
            queued_sequences: VecDeque::new(),
            process: 0,
            finished_glyphs: false,
        }
    }

    /// Apply a speed multiplier to the base speed of the [`Typewriter`].
    ///
    /// The base speed is supplied by the [`Typewriter::new`] constructor.
    pub fn apply_speed_mult(&mut self, mult: f32) {
        let speed = self.speed;
        self.timer
            .set_duration(Duration::from_secs_f32(1. / speed / mult));
    }

    fn new_timer(speed: f32) -> Timer {
        let dur = 1.0 / speed;
        let mut timer = Timer::from_seconds(dur, TimerMode::Repeating);
        timer.set_elapsed(Duration::from_secs_f32(dur));

        timer
    }

    fn queue_sequences(
        &mut self,
        index: &TypewriterIndex,
        words: &Words,
        children: &Children,
        spans: Query<(Entity, &SpanGlyphs)>,
    ) {
        if children.is_empty() {
            return;
        }

        let target_index = if self.finished_glyphs {
            children.len()
        } else {
            let index = index.glyph_index(words);
            let mut offset = 0;
            let target_entity = spans
                .iter_many(children)
                .find_map(|(entity, span)| {
                    if span.len() + offset > index {
                        Some(entity)
                    } else {
                        offset += span.len();
                        None
                    }
                })
                .unwrap_or_else(|| children.last().copied().unwrap());
            children.iter().position(|e| e == target_entity).unwrap()
        };

        for entity in children
            .iter()
            .take(target_index + 1)
            .skip(self.completed_sequences)
        {
            if !self.queued_sequences.contains(&entity) {
                self.queued_sequences.push_back(entity);
            }
        }
        self.process = self.queued_sequences.len();
    }

    fn process_sequence(&self) -> impl Iterator<Item = Entity> {
        self.queued_sequences.iter().copied().take(self.process)
    }

    fn flush_sequence_queue(&mut self) {
        self.completed_sequences += self.process;
        for _ in 0..self.process {
            self.queued_sequences.pop_front();
        }
        self.process = 0;
    }
}

/// Stores the current index into a [`Typewriter`]s text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub enum TypewriterIndex {
    /// A glyph of a font, typically representing a single character.
    ///
    /// Indexes into [`Glyphs`].
    Glyph(usize),

    /// A collection of [`Glyph`]s.
    ///
    /// Indexes into [`Words`].
    Word(usize),
}

impl Default for TypewriterIndex {
    fn default() -> Self {
        Self::glyph()
    }
}

impl TypewriterIndex {
    /// Create a new [`TypewriterIndex::Glyph`] with index `0`.
    pub fn glyph() -> Self {
        Self::Glyph(0)
    }

    /// Create a new [`TypewriterIndex::Word`] with index `0`.
    pub fn word() -> Self {
        Self::Word(0)
    }

    /// Returns the absolute index into [`Glyphs`].
    pub fn glyph_index(&self, words: &Words) -> usize {
        match self {
            TypewriterIndex::Glyph(index) => *index,
            TypewriterIndex::Word(index) => words.glyph_ranges()[*index].end.saturating_sub(1),
        }
    }

    /// Increment the index by `1`.
    pub fn incr(&mut self) {
        match self {
            TypewriterIndex::Glyph(index) | TypewriterIndex::Word(index) => *index += 1,
        }
    }
}

/// An event triggered by a [`Typewriter`] entity when a [`Glyph`] is revealed.
///
/// A [`Typewriter`] configured to [`TypewriterIndex::Word`] will emit a [`GlyphRevealed`]
/// for all [`WordRevealed::glyphs`].
#[derive(Debug, Clone, Event, Reflect)]
pub struct GlyphRevealed {
    /// The revealed [`Glyph`].
    pub glyph: Entity,

    /// The [`Glyph`]'s text.
    pub text: String,
}

/// An event triggered by a [`Typewriter`] entity when a word is revealed.
///
/// A [`Typewriter`] configured to [`TypewriterIndex::Word`] will emit a [`GlyphRevealed`]
/// for all [`WordRevealed::glyphs`].
#[derive(Debug, Clone, Event, Reflect)]
pub struct WordRevealed {
    /// The revealed collection of [`Glyph`]s.
    pub glyphs: Vec<Entity>,

    /// The word's text.
    pub text: String,
}

/// Pause the execution of a [`Typewriter`].
#[derive(Debug, Clone, Component, Reflect)]
pub struct PauseTypewriter(pub Timer);

impl PauseTypewriter {
    /// Creates a new type writer pause with the given duration in seconds.
    #[inline]
    pub fn from_seconds(duration: f32) -> Self {
        Self(Timer::from_seconds(duration, TimerMode::Once))
    }
}

fn pause(
    mut commands: Commands,
    time: Res<Time>,
    mut typewriters: Query<(Entity, &mut PauseTypewriter), With<Typewriter>>,
) {
    for (entity, mut pause) in typewriters.iter_mut() {
        pause.0.tick(time.delta());
        if pause.0.finished() {
            commands.entity(entity).remove::<PauseTypewriter>();
        }
    }
}

fn step(
    mut commands: Commands,
    time: Res<Time>,
    mut typewriters: Query<
        (
            Entity,
            &mut Typewriter,
            &mut TypewriterIndex,
            &Words,
            &Glyphs,
            Has<Initialize>,
        ),
        Without<PauseTypewriter>,
    >,
    reader: GlyphReader,
) -> Result {
    for (entity, mut typewriter, mut index, words, glyphs, init) in typewriters.iter_mut() {
        if init {
            commands
                .entity(entity)
                .remove::<Initialize>()
                .insert(Sequence);
            continue;
        }

        match *index {
            TypewriterIndex::Glyph(glyph) => {
                if glyph >= glyphs.len() {
                    *index = TypewriterIndex::Word(glyphs.len());
                    typewriter.finished_glyphs = true;
                    continue;
                }
            }
            TypewriterIndex::Word(word) => {
                if word >= words.len() {
                    *index = TypewriterIndex::Word(words.len());
                    typewriter.finished_glyphs = true;
                    continue;
                }
            }
        }

        typewriter.timer.tick(time.delta());
        if !typewriter.timer.just_finished() {
            continue;
        };

        match *index {
            TypewriterIndex::Glyph(index) => {
                let glyph = glyphs.collection()[index];

                // reveal glyph first
                commands.entity(entity).trigger(GlyphRevealed {
                    glyph,
                    text: reader.read(glyph)?.to_string(),
                });

                // then word
                if let Some(range) = words
                    .glyph_ranges()
                    .iter()
                    .find(|range| range.end == index + 1)
                {
                    let glyphs = &glyphs.collection()[range.clone()];
                    commands.entity(entity).trigger(WordRevealed {
                        glyphs: glyphs.to_vec(),
                        text: glyphs
                            .iter()
                            .copied()
                            .map(|g| reader.read(g))
                            .collect::<Result<Vec<_>, _>>()?
                            .join(""),
                    });
                }

                if index + 1 == glyphs.len() {
                    typewriter.finished_glyphs = true;
                }
            }
            TypewriterIndex::Word(index) => {
                let word = &words.glyph_ranges()[index];
                let glyphs = &glyphs.collection()[word.clone()];

                // reveal glyphs first
                for &glyph in glyphs.iter() {
                    commands.entity(entity).trigger(GlyphRevealed {
                        glyph,
                        text: reader.read(glyph)?.to_string(),
                    });
                }

                // then word
                commands.entity(entity).trigger(WordRevealed {
                    glyphs: glyphs.to_vec(),
                    text: glyphs
                        .iter()
                        .copied()
                        .map(|g| reader.read(g))
                        .collect::<Result<Vec<_>, _>>()?
                        .join(""),
                });

                if index + 1 == words.len() {
                    typewriter.finished_glyphs = true;
                }
            }
        }
        if !typewriter.finished_glyphs {
            index.incr();
        }
        commands.entity(entity).insert(Sequence);
    }
    Ok(())
}

/// [`Typewriter`] marker component for applying any queued [`TypewriterCommand`]s,
/// [`TypewriterEvent`]s, and [`TypewriterCallback`]s.
#[derive(Debug, Default, Component)]
pub struct Sequence;

fn start_sequence(
    mut typewriters: Query<
        (&mut Typewriter, &TypewriterIndex, &Words, &Children),
        (With<Sequence>, Without<PauseTypewriter>),
    >,
    spans: Query<(Entity, &SpanGlyphs)>,
) {
    for (mut typewriter, index, words, children) in typewriters.iter_mut() {
        typewriter.queue_sequences(index, words, children, spans);
    }
}

fn end_sequence(
    mut commands: Commands,
    mut typewriters: Query<(Entity, &mut Typewriter, Has<PauseTypewriter>), With<Sequence>>,
) {
    for (entity, mut typewriter, paused) in typewriters.iter_mut() {
        typewriter.flush_sequence_queue();
        if !paused && typewriter.queued_sequences.is_empty() {
            commands.entity(entity).remove::<Sequence>();
            if typewriter.finished_glyphs {
                commands.entity(entity).insert(FinishTypewriter);
            }
        }
    }
}

/// Disable [`TypewriterCommand`]s from applying to a [`Typewriter`].
#[derive(Debug, Default, Component)]
pub struct DisableCommands;

fn commands(
    mut commands: Commands,
    mut typewriters: Query<
        (Entity, &mut Typewriter),
        (
            With<Sequence>,
            Without<DisableCommands>,
            Without<PauseTypewriter>,
        ),
    >,
    command_q: Query<&TypewriterCommand>,
) {
    for (entity, mut typewriter) in typewriters.iter_mut() {
        let mut speed_mult = None;
        for (i, &e) in typewriter.queued_sequences.iter().enumerate() {
            if let Ok(command) = command_q.get(e) {
                match *command {
                    TypewriterCommand::Pause(dur) => {
                        commands
                            .entity(entity)
                            .insert(PauseTypewriter::from_seconds(dur));
                        typewriter.process = i + 1;
                        break;
                    }
                    TypewriterCommand::Speed(mult) => {
                        speed_mult = Some(mult);
                    }
                }
            }
        }
        if let Some(mult) = speed_mult {
            typewriter.apply_speed_mult(mult);
        }
    }
}

/// Disable [`TypewriterEvent`]s from emitting.
#[derive(Debug, Default, Component)]
pub struct DisableEvents;

fn events(
    mut commands: Commands,
    typewriters: Query<
        (Entity, &Typewriter, &Children, Has<FinishTypewriter>),
        (With<Sequence>, Without<DisableEvents>),
    >,
    event_q: Query<&TypewriterEvent>,
    mut writer: EventWriter<TypewriterEvent>,
) {
    for (entity, typewriter, children, finish) in typewriters.iter() {
        if finish {
            for event in event_q.iter_many(children.iter().skip(typewriter.completed_sequences)) {
                let event = TypewriterEvent(event.0.clone());
                writer.write(event.clone());
                commands.entity(entity).trigger(event.clone());
            }
            continue;
        }

        for event in event_q.iter_many(typewriter.process_sequence()) {
            let event = TypewriterEvent(event.0.clone());
            writer.write(event.clone());
            commands.entity(entity).trigger(event.clone());
        }
    }
}

/// Disable [`TypewriterCallback`]s from running.
#[derive(Debug, Default, Component)]
pub struct DisableCallbacks;

fn callbacks(
    mut commands: Commands,
    typewriters: Query<
        (&Typewriter, &Children, Has<FinishTypewriter>),
        (With<Sequence>, Without<DisableCallbacks>),
    >,
    callback_q: Query<&TypewriterCallback>,
) {
    for (typewriter, children, finish) in typewriters.iter() {
        if finish {
            for callback in
                callback_q.iter_many(children.iter().skip(typewriter.completed_sequences))
            {
                callback.queue(&mut commands);
            }
        } else {
            for callback in callback_q.iter_many(typewriter.process_sequence()) {
                callback.queue(&mut commands);
            }
        }
    }
}

/// An event triggered by a [`Typewriter`] entity when the entire text hierarchy is revealed.
#[derive(Debug, Clone, Copy, Event, Reflect)]
pub struct TypewriterFinished;

/// Finishes a [`Typewriter`].
///
/// All remaining glyphs will be revealed, and events and callbacks will be triggered.
/// Alternatively, use [`ShortCircuitTypewriter`].
#[derive(Debug, Default, Component)]
pub struct FinishTypewriter;

fn finish(
    mut commands: Commands,
    typewriters: Query<
        (Entity, &Glyphs),
        (
            With<Typewriter>,
            With<FinishTypewriter>,
            Without<PauseTypewriter>,
        ),
    >,
    mut visibility: Query<&mut Visibility>,
) -> Result {
    for (entity, glyphs) in typewriters.iter() {
        for entity in glyphs.iter() {
            let mut vis = visibility.get_mut(entity)?;
            if *vis != Visibility::Inherited {
                *vis = Visibility::Inherited;
            }
        }

        commands
            .entity(entity)
            .remove::<(
                Initialize,
                Sequence,
                Typewriter,
                TypewriterIndex,
                PauseTypewriter,
                FinishTypewriter,
                DisableCommands,
                DisableEvents,
                DisableCallbacks,
                ShortCircuitTypewriter,
            )>()
            .trigger(TypewriterFinished);
    }
    Ok(())
}

/// Finishes a [`Typewriter`] without events or callbacks.
///
/// All remaining glyphs will be revealed, and events and callbacks will not be triggered.
/// Alternatively, use [`FinishTypewriter`].
#[derive(Debug, Default, Component)]
#[require(FinishTypewriter, DisableCommands, DisableEvents, DisableCallbacks)]
pub struct ShortCircuitTypewriter;

fn reveal_glyph(
    trigger: Trigger<GlyphRevealed>,
    mut commands: Commands,
    mut visibility: Query<&mut Visibility>,
) -> Result {
    commands.entity(trigger.glyph).insert(Appeared(0.0));
    let mut vis = visibility.get_mut(trigger.glyph)?;
    if *vis != Visibility::Inherited {
        *vis = Visibility::Inherited;
    }
    Ok(())
}

/// [`Typewriter`] marker component that will set all [`Glyph`] visibilities to
/// [`Visibility::Hidden`] and apply any sequencing specified before the first glyph.
#[derive(Debug, Default, Component)]
pub struct Initialize;

// NOTE: `Initialize` is removed in step.
fn initialize(
    mut commands: Commands,
    hide: Query<&Glyphs, With<Initialize>>,
    mut glyph: Query<(&mut Visibility, Has<Appeared>)>,
) -> Result {
    for glyphs in hide.iter() {
        for entity in glyphs.iter() {
            let (mut vis, appeared) = glyph.get_mut(entity)?;
            if *vis != Visibility::Hidden {
                *vis = Visibility::Hidden;
            }
            if appeared {
                commands.entity(entity).remove::<Appeared>();
            }
        }
    }
    Ok(())
}
