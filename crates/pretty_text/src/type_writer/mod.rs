//! Provides the beloved **type writer** text effect with additional
//! sequencing including:
//! - Changing speed
//! - Pausing
//! - Emitting events
//! - Running one shot systems
//!
//! For more detail, see [`TypeWriter`].

use std::ops::Range;
use std::time::Duration;

use bevy::ecs::entity::EntityHashMap;
use bevy::prelude::*;
use bevy::render::view::VisibilitySystems;
use bevy::text::ComputedTextBlock;

use crate::PrettyText;
use crate::glyph::{Glyph, GlyphOf, GlyphSpanEntity, GlyphSystems, Glyphs};

use hierarchy::{TypeWriterCallback, TypeWriterCommand, TypeWriterEvent};

pub mod hierarchy;

/// A [`SystemSet`] for [`TypeWriter`] systems.
///
/// Runs in the [`Update`] schedule.
#[derive(Debug, Default, Clone, Copy, SystemSet, Eq, PartialEq, Hash)]
pub struct TypeWriterSet;

/// A plugin for managing [`TypeWriter`] entities.
#[derive(Debug)]
pub struct TypeWriterPlugin;

impl Plugin for TypeWriterPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<GlyphRevealed>()
            .add_event::<TypeWriterFinished>()
            .add_event::<TypeWriterEvent>()
            .register_type::<TypeWriterCommand>()
            .add_systems(
                Update,
                (calculate_byte_range, type_writer)
                    .chain()
                    .in_set(TypeWriterSet),
            )
            .add_systems(
                PostUpdate,
                reveal_glyphs
                    .after(GlyphSystems::Construct)
                    .before(VisibilitySystems::VisibilityPropagate),
            )
            .add_observer(removed_reveal);

        app.register_type::<TypeWriter>()
            .register_type::<TypeWriterMode>()
            .register_type::<TypeWriterFinished>()
            .register_type::<GlyphRevealed>()
            .register_type::<PauseTypeWriter>()
            .register_type::<Reveal>()
            .register_type::<TypeWriterCommand>()
            .register_type::<TypeWriterEvent>()
            .register_type::<DisableCommands>();
    }
}

/// Controls the visibility of [`Glyph`]s in a text hierarchy.
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
/// # let mut world = World::new();
/// // Reveal 3 glyphs.
/// world.spawn((
///     Reveal(3),
///     Text::new("my text"),
/// ));
///
/// // Hide all glyphs.
/// world.spawn((
///     Reveal::NONE,
///     Text::new("my text"),
/// ));
/// ```
#[derive(Debug, Default, Clone, Copy, Deref, DerefMut, Component, Reflect)]
#[require(PrettyText)]
pub struct Reveal(pub usize);

impl Reveal {
    /// Reveal all [`Glyph`]s in a text hierarchy.
    pub const ALL: Self = Self(usize::MAX);

    /// Hide all [`Glyph`]s in a text hierarchy.
    pub const NONE: Self = Self(0);
}

/// [`TypeWriter`] reveals text over time.
///
/// Placing [`TypeWriter`] in a [`Text`] or [`Text2d`] entity will immediately hide and
/// begin revealing text. [Example usage.]
///
/// [Example usage.]: https://github.com/void-scape/pretty-text/blob/c3cc5163625b1d12912f919b2b8c95a525ddcfbe/crates/plugin/examples/type_writer.rs
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     TypeWriter::new(30.0),
///     Text::new("my text"),
/// ));
///
/// // With configuration.
/// world.spawn((
///     TypeWriter::new(2.0),
///     TypeWriterMode::Word,
///     Text::new("my text"),
/// ));
/// ```
///
/// # Special Sequencing
///
/// The [`TypeWriter`] has special syntax for creating sequencing effects. These
/// effects include:
/// - Changing speed
/// - Pausing
/// - Emitting events
/// - Running one shot systems
///
/// For the special type writer syntax, see [`parser`](crate::parser#type-writer-syntax).
///
/// To understand how the effects are represented in the ECS, see [`hierarchy`].
///
/// # Revealing Text
///
/// The [`TypeWriter`] uses the [`Reveal`] component to control how many glyphs are visible at a time.
/// [`TypeWriter`]s can reveal either glyphs (the default) or words, configurable with [`TypeWriterMode`].
///
/// The [`TypeWriter`] entity will trigger events related to the revealed text:
/// - [`GlyphRevealed`] (when configured with [`TypeWriterMode::Glyph`])
/// - [`WordRevealed`] (when configured with [`TypeWriterMode::Word`])
///
/// You can observe these events to, for example, play a sound:
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
#[doc = include_str!("../../docs/audio_player.txt")]
/// #
/// # let mut world = World::new();
/// world
///     .spawn((
///         TypeWriter::new(30.0),
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
/// Once a `TypeWriter` has revealed the entire text hierarchy, the entity will remove its
/// `TypeWriter` related components (`TypeWriter`, [`TypeWriterMode`], [`Reveal`]) and
/// trigger the [`TypeWriterFinished`] event.
///
/// # Early Completion
///
/// In some cases it is useful to advance the [`TypeWriter`] to the end of the sequence.
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
/// fn short_circuit(mut commands: Commands, typewriter: Single<Entity, With<TypeWriter>>) {
///     // Removing the typewriter will reveal remaining glyphs but *skip*
///     // any untriggered events or callbacks.
///     commands.entity(*typewriter).remove::<TypeWriter>();
/// }
///
/// fn finish(mut typewriter: Single<&mut TypeWriter>) {
///     // Finishing the typewriter will reveal remaining gylphs *and* trigger
///     // remaining events and or callbacks.
///     typewriter.finish();
/// }
/// ```
///
/// In both cases, a [`TypeWriterFinished`] event will be triggered.
#[derive(Debug, Clone, Component, Reflect)]
#[require(PrettyText, TypeWriterMode, Reveal)]
pub struct TypeWriter {
    speed: f32,
    timer: Timer,
    processed_children: Vec<Entity>,
    finish: bool,
}

impl TypeWriter {
    /// Creates a new `TypeWriter` with `speed`.
    ///
    /// The unit of `speed` is:
    /// - glyphs/second (when configured with [`TypeWriterMode::Glyph`])
    /// - words/second (when configured with [`TypeWriterMode::Word`])
    #[inline]
    pub fn new(speed: f32) -> Self {
        Self {
            speed,
            timer: Self::new_timer(speed),
            processed_children: Vec::new(),
            finish: false,
        }
    }

    /// Apply a speed multiplier to the base speed of the [`TypeWriter`].
    ///
    /// The base speed is supplied by the [`TypeWriter::new`] constructor.
    pub fn apply_speed_mult(&mut self, mult: f32) {
        let speed = self.speed;
        self.timer
            .set_duration(Duration::from_secs_f32(1. / speed / mult));
    }

    /// Finishes the `TypeWriter`.
    ///
    /// All remaining glyphs will be revealed, and events and
    /// callbacks will be triggered.
    ///
    /// If this is not desirable, consider removing the `TypeWriter`
    /// component. Removal will instead short circuit the sequence and
    /// not trigger any remaining events or callbacks.
    pub fn finish(&mut self) {
        self.finish = true;
    }

    #[inline]
    fn new_timer(speed: f32) -> Timer {
        let dur = 1.0 / speed;
        let mut timer = Timer::from_seconds(dur, TimerMode::Repeating);
        timer.set_elapsed(Duration::from_secs_f32(dur));

        timer
    }
}

/// Configures the unit of text revealed by a [`TypeWriter`].
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub enum TypeWriterMode {
    /// A glyph of a font, typically representing a single character.
    ///
    /// Represented by a single [`Glyph`] entity.
    #[default]
    Glyph,

    /// A collection of [`Glyph`]s.
    Word,
}

/// Disable [`TypeWriterCommand`]s from applying to a [`TypeWriter`].
#[derive(Debug, Default, Component, Reflect)]
pub struct DisableCommands;

/// An event triggered by a [`TypeWriter`] entity when a [`Glyph`] is revealed.
///
/// `GlyphRevealed` is only triggered when the [`TypeWriter`] entity is configured with
/// [`TypeWriterMode::Glyph`].
#[derive(Debug, Clone, Event, Reflect)]
pub struct GlyphRevealed {
    /// The revealed [`Glyph`].
    ///
    /// `glyph` can be `None` when the original text is whitespace and stripped from the
    /// underlying cosmic buffer.
    pub glyph: Option<Entity>,

    /// The text represented by the [`Glyph`].
    pub text: String,
}

/// An event triggered by a [`TypeWriter`] entity when a word is revealed.
///
/// `WordRevealed` is only triggered when the [`TypeWriter`] entity is configured with
/// [`TypeWriterMode::Word`].
#[derive(Debug, Clone, Event, Reflect)]
pub struct WordRevealed {
    /// The revealed collection of [`Glyph`]s.
    pub glyphs: Vec<Entity>,

    /// The text represented by the word.
    pub text: String,
}

/// An event triggered by a [`TypeWriter`] entity when the entire text hierarchy is revealed.
#[derive(Debug, Clone, Copy, Event, Reflect)]
pub struct TypeWriterFinished;

/// Pause the execution of a [`TypeWriter`].
#[derive(Debug, Clone, Component, Reflect)]
pub struct PauseTypeWriter(pub Timer);

impl PauseTypeWriter {
    /// Creates a new type writer pause with the given duration in seconds.
    #[inline]
    pub fn from_seconds(duration: f32) -> Self {
        Self(Timer::from_seconds(duration, TimerMode::Once))
    }
}

/// Update [`Glyph`] visibility based on the state of [`Reveal`].
pub fn reveal_glyphs(
    reveal: Query<
        (&Glyphs, &ComputedTextBlock, &Reveal),
        Or<(Changed<Reveal>, Added<Reveal>, Changed<Glyphs>)>,
    >,
    mut visibilities: Query<(&Glyph, &mut Visibility), With<GlyphOf>>,
) {
    for (glyphs, block, reveal) in reveal.iter() {
        for entity in glyphs.iter() {
            if let Ok((glyph, mut glyph_visibility)) = visibilities.get_mut(entity) {
                let line_offset = block
                    .buffer()
                    .lines
                    .iter()
                    .take(glyph.0.line_index)
                    .map(|line| line.text().chars().count())
                    .sum::<usize>();

                let target = if line_offset + glyph.0.byte_index + glyph.0.byte_length <= reveal.0 {
                    Visibility::Inherited
                } else {
                    Visibility::Hidden
                };

                if *glyph_visibility != target {
                    *glyph_visibility = target;
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
            if let Ok(mut vis) = visibilities.get_mut(entity)
                && *vis != Visibility::Inherited
            {
                *vis = Visibility::Inherited;
            }
        }
    }
}

#[derive(Default, Component)]
struct ByteRange(Range<usize>);

fn calculate_byte_range(
    mut commands: Commands,
    blocks: Query<(&Glyphs, &ComputedTextBlock), Or<(Changed<Glyphs>, Added<Glyphs>)>>,
    spans_entities: Query<(&Glyph, &GlyphSpanEntity)>,
) -> Result {
    let mut span_indices = EntityHashMap::<Vec<_>>::default();
    for (glyphs, block) in blocks.iter() {
        for entity in glyphs.iter() {
            let (glyph, span_entity) = spans_entities.get(entity)?;
            span_indices.entry(span_entity.0).or_default().push((
                (
                    glyph.0.line_index,
                    glyph.0.byte_index,
                    glyph.0.byte_index + glyph.0.byte_length,
                ),
                block,
            ));
        }
    }

    for (span_entity, byte_ranges) in span_indices.into_iter() {
        if byte_ranges.is_empty() {
            continue;
        }

        let start = byte_ranges
            .iter()
            .map(|((line, start, _), block)| {
                block
                    .buffer()
                    .lines
                    .iter()
                    .take(*line)
                    .map(|line| line.text().chars().count())
                    .sum::<usize>()
                    + *start
            })
            .min()
            .unwrap();
        let end = byte_ranges
            .iter()
            .map(|((line, _, end), block)| {
                block
                    .buffer()
                    .lines
                    .iter()
                    .take(*line)
                    .map(|line| line.text().chars().count())
                    .sum::<usize>()
                    + *end
            })
            .max()
            .unwrap();
        commands.entity(span_entity).insert(ByteRange(start..end));
    }

    Ok(())
}

fn type_writer(
    mut commands: Commands,
    time: Res<Time>,
    mut type_writers: Query<(
        Entity,
        &Glyphs,
        &ComputedTextBlock,
        &TypeWriterMode,
        &mut TypeWriter,
        Mut<Reveal>,
        Option<&mut PauseTypeWriter>,
        Option<&Children>,
        Has<DisableCommands>,
    )>,
    mut writer: EventWriter<TypeWriterEvent>,
    glyph_query: Query<&Glyph>,
    spans: Query<&ByteRange, With<TextSpan>>,
    effects: Query<&TypeWriterCommand>,
    events: Query<&TypeWriterEvent>,
    callbacks: Query<&TypeWriterCallback>,
) -> Result {
    for (entity, glyphs, block, mode, mut tw, mut reveal, pause, children, commands_disabled) in
        type_writers.iter_mut()
    {
        if tw.finish {
            if let Some(children) = children {
                for child in children.iter() {
                    if tw.processed_children.contains(&child) {
                        continue;
                    }

                    if let Ok(effect) = effects.get(child) {
                        if !commands_disabled {
                            match *effect {
                                TypeWriterCommand::Pause(dur) => {
                                    commands
                                        .entity(entity)
                                        .insert(PauseTypeWriter::from_seconds(dur));
                                }
                                TypeWriterCommand::Speed(mult) => {
                                    tw.apply_speed_mult(mult);
                                }
                            }
                        }
                    }
                    //
                    else if let Ok(callback) = callbacks.get(child) {
                        callback.queue(&mut commands);
                    }
                    //
                    else if let Ok(event) = events.get(child) {
                        let event = TypeWriterEvent(event.0.clone());
                        writer.write(event.clone());
                        commands.entity(entity).trigger(event.clone());
                    }
                }
            }

            commands
                .entity(entity)
                .remove::<(TypeWriter, TypeWriterMode, Reveal)>()
                .trigger(TypeWriterFinished);
            continue;
        }

        if let Some(mut pause) = pause {
            pause.0.tick(time.delta());
            if pause.0.finished() {
                commands.entity(entity).remove::<PauseTypeWriter>();
            } else {
                continue;
            }
        }

        // TODO: this doesn't need to happen every update
        let mut should_pause = false;
        if let Some(children) = children {
            for child in children.iter() {
                if tw.processed_children.contains(&child) {
                    continue;
                }

                if let Ok(range) = spans.get(child) {
                    if reveal.0 == 0 || range.0.end > reveal.0 {
                        break;
                    }
                    continue;
                }
                //
                else if let Ok(effect) = effects.get(child) {
                    tw.processed_children.push(child);
                    if !commands_disabled {
                        match *effect {
                            TypeWriterCommand::Pause(dur) => {
                                commands
                                    .entity(entity)
                                    .insert(PauseTypeWriter::from_seconds(dur));
                                should_pause = true;
                                break;
                            }
                            TypeWriterCommand::Speed(mult) => {
                                tw.apply_speed_mult(mult);
                            }
                        }
                    }
                }
                //
                else if let Ok(callback) = callbacks.get(child) {
                    tw.processed_children.push(child);
                    callback.queue(&mut commands);
                }
                //
                else if let Ok(event) = events.get(child) {
                    tw.processed_children.push(child);
                    let event = TypeWriterEvent(event.0.clone());
                    writer.write(event.clone());
                    commands.entity(entity).trigger(event.clone());

                    continue;
                }
            }
        }

        if should_pause {
            continue;
        }

        let mut accum = 0;
        let mut line_offset = 0;
        let line_index = block
            .buffer()
            .lines
            .iter()
            .take_while(|line| {
                accum += line.text().chars().count();
                let take = accum <= reveal.0;
                if take {
                    line_offset += line.text().chars().count();
                }
                take
            })
            .count();

        if reveal.0 >= accum {
            commands
                .entity(entity)
                .remove::<(TypeWriter, TypeWriterMode, Reveal)>()
                .trigger(TypeWriterFinished);
            continue;
        }

        tw.timer.tick(time.delta());
        if tw.timer.just_finished() {
            match mode {
                TypeWriterMode::Glyph => {
                    let text = block
                        .buffer()
                        .lines
                        .get(line_index)
                        .map(|line| {
                            line.text()
                                .chars()
                                .skip(reveal.0 - line_offset)
                                .take(line_offset + 1)
                                .collect()
                        })
                        // TODO: try again next frame instead?
                        .ok_or("`ComputedTextBlock` buffer is empty")?;

                    commands.entity(entity).trigger(GlyphRevealed {
                        glyph: glyphs
                            .iter()
                            .flat_map(|glyph| glyph_query.get(glyph).map(|g| (glyph, g)).ok())
                            .find_map(|(entity, glyph)| {
                                (glyph.0.byte_index + glyph.0.byte_length
                                    == reveal.0 - line_offset + 1)
                                    .then_some(entity)
                            }),
                        text,
                    });

                    reveal.0 += 1;
                }
                TypeWriterMode::Word => {
                    let text = block
                        .buffer()
                        .lines
                        .get(line_index)
                        .map(|line| line.text())
                        // TODO: try again next frame instead?
                        .ok_or("`ComputedTextBlock` buffer is empty")?;

                    let start = reveal.0 - line_offset;
                    let word_slice = &text[start..];
                    let word_end_offset = word_slice
                        .chars()
                        .position(|c| c.is_whitespace())
                        .unwrap_or(word_slice.chars().count());

                    let end = start + word_end_offset;
                    let mut last_char = text[start..].chars().next().unwrap();

                    match text[start..].chars().position(|char| {
                        let next_word = last_char.is_whitespace() && !char.is_whitespace();
                        last_char = char;
                        next_word
                    }) {
                        Some(next_space) => reveal.0 += next_space,
                        None => reveal.0 = accum,
                    }

                    debug_assert!(
                        !text[start..end].chars().any(char::is_whitespace),
                        "revealed word contains whitespace: `{}`",
                        &text[start..end]
                    );

                    commands.entity(entity).trigger(WordRevealed {
                        glyphs: glyphs
                            .iter()
                            .flat_map(|glyph| glyph_query.get(glyph).map(|g| (glyph, g)).ok())
                            .filter_map(|(entity, glyph)| {
                                let glyph_start = glyph.0.byte_index;
                                let glyph_end = glyph.0.byte_index + glyph.0.byte_length;
                                (glyph_start < end && glyph_end > start).then_some(entity)
                            })
                            .collect(),
                        text: text[start..end].to_string(),
                    });
                }
            }
        }
    }

    Ok(())
}
