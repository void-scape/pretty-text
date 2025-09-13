//! Provides the beloved **type writer** text effect with additional
//! sequencing including:
//! - Changing speed
//! - Pausing
//! - Emitting events
//! - Running one shot systems
//!
//! For more detail, see [`Typewriter`].

use std::ops::Range;
use std::time::Duration;

use bevy::ecs::entity::EntityHashMap;
use bevy::ecs::query::QueryFilter;
use bevy::prelude::*;
use bevy::render::view::VisibilitySystems;
use bevy::text::ComputedTextBlock;

use crate::PrettyText;
use crate::effects::appearance::Appeared;
use crate::glyph::{Glyph, GlyphOf, GlyphSystems, Glyphs, SpanGlyphOf};

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
                    calculate_byte_range,
                    typewriter,
                    reveal_glyphs::<Changed<Reveal>>,
                )
                    .chain()
                    .in_set(TypewriterSet),
            )
            .add_systems(
                PostUpdate,
                reveal_glyphs::<Or<(Added<Reveal>, Changed<Glyphs>)>>
                    .after(GlyphSystems::Construct)
                    .before(VisibilitySystems::VisibilityPropagate),
            )
            .add_observer(removed_reveal);

        app.register_type::<Typewriter>()
            .register_type::<TypewriterMode>()
            .register_type::<TypewriterFinished>()
            .register_type::<GlyphRevealed>()
            .register_type::<PauseTypewriter>()
            .register_type::<Reveal>()
            .register_type::<TypewriterCommand>()
            .register_type::<TypewriterEvent>()
            .register_type::<DisableCommands>();
    }
}

/// Controls the visibility of [`Glyph`]s in a text hierarchy.
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # use bevy_pretty_text::typewriter::*;
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

/// [`Typewriter`] reveals text over time.
///
/// Placing [`Typewriter`] in a [`Text`] or [`Text2d`] entity will immediately hide and
/// begin revealing text. [Example usage.]
///
/// [Example usage.]: https://github.com/void-scape/pretty-text/blob/c3cc5163625b1d12912f919b2b8c95a525ddcfbe/crates/plugin/examples/typewriter.rs
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
/// // With configuration.
/// world.spawn((
///     Typewriter::new(2.0),
///     TypewriterMode::Word,
///     Text::new("my text"),
/// ));
/// ```
///
/// # Special Sequencing
///
/// The [`Typewriter`] has special syntax for creating sequencing effects. These
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
/// The [`Typewriter`] uses the [`Reveal`] component to control how many glyphs are visible at a time.
/// [`Typewriter`]s can reveal either glyphs (the default) or words, configurable with [`TypewriterMode`].
///
/// The [`Typewriter`] entity will trigger events related to the revealed text:
/// - [`GlyphRevealed`] (when configured with [`TypewriterMode::Glyph`])
/// - [`WordRevealed`] (when configured with [`TypewriterMode::Word`])
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
/// Once a `Typewriter` has revealed the entire text hierarchy, the entity will remove its
/// `Typewriter` related components (`Typewriter`, [`TypewriterMode`], [`Reveal`]) and
/// trigger the [`TypewriterFinished`] event.
///
/// # Early Completion
///
/// In some cases it is useful to advance the [`Typewriter`] to the end of the sequence.
///
/// ```
/// fn finish(mut typewriter: Single<&mut Typewriter>) {
///     // Finishing the typewriter will reveal remaining gylphs *and* trigger
///     // remaining events and or callbacks.
///     typewriter.finish();
/// }
///
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// fn short_circuit(mut commands: Commands, typewriter: Single<Entity, With<Typewriter>>) {
///     // Removing the typewriter will reveal remaining glyphs but *skip*
///     // any untriggered events or callbacks.
///     commands.entity(*typewriter).remove::<Typewriter>();
/// }
/// ```
///
/// In both cases, a [`TypewriterFinished`] event will be triggered.
#[derive(Debug, Clone, Component, Reflect)]
#[require(PrettyText, TypewriterMode, Reveal)]
pub struct Typewriter {
    speed: f32,
    timer: Timer,
    processed_children: Vec<Entity>,
    finish: bool,
}

impl Typewriter {
    /// Creates a new `Typewriter` with `speed`.
    ///
    /// The unit of `speed` is:
    /// - glyphs/second (when configured with [`TypewriterMode::Glyph`])
    /// - words/second (when configured with [`TypewriterMode::Word`])
    #[inline]
    pub fn new(speed: f32) -> Self {
        Self {
            speed,
            timer: Self::new_timer(speed),
            processed_children: Vec::new(),
            finish: false,
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

    /// Finishes the `Typewriter`.
    ///
    /// All remaining glyphs will be revealed, and events and
    /// callbacks will be triggered.
    ///
    /// If this is not desirable, consider removing the `Typewriter`
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

/// Configures the unit of text revealed by a [`Typewriter`].
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub enum TypewriterMode {
    /// A glyph of a font, typically representing a single character.
    ///
    /// Represented by a single [`Glyph`] entity.
    #[default]
    Glyph,

    /// A collection of [`Glyph`]s.
    Word,
}

/// Disable [`TypewriterCommand`]s from applying to a [`Typewriter`].
#[derive(Debug, Default, Component, Reflect)]
pub struct DisableCommands;

/// An event triggered by a [`Typewriter`] entity when a [`Glyph`] is revealed.
///
/// `GlyphRevealed` is only triggered when the [`Typewriter`] entity is configured with
/// [`TypewriterMode::Glyph`].
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

/// An event triggered by a [`Typewriter`] entity when a word is revealed.
///
/// `WordRevealed` is only triggered when the [`Typewriter`] entity is configured with
/// [`TypewriterMode::Word`].
#[derive(Debug, Clone, Event, Reflect)]
pub struct WordRevealed {
    /// The revealed collection of [`Glyph`]s.
    pub glyphs: Vec<Entity>,

    /// The text represented by the word.
    pub text: String,
}

/// An event triggered by a [`Typewriter`] entity when the entire text hierarchy is revealed.
#[derive(Debug, Clone, Copy, Event, Reflect)]
pub struct TypewriterFinished;

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

/// Update [`Glyph`] visibility based on the state of [`Reveal`].
pub fn reveal_glyphs<F: QueryFilter>(
    mut commands: Commands,
    reveal: Query<(&Glyphs, &ComputedTextBlock, &Reveal), F>,
    mut visibilities: Query<(Entity, &Glyph, &mut Visibility), With<GlyphOf>>,
) {
    for (glyphs, block, reveal) in reveal.iter() {
        for entity in glyphs.iter() {
            if let Ok((entity, glyph, mut glyph_visibility)) = visibilities.get_mut(entity) {
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
                    if target == Visibility::Inherited {
                        // NOTE: The inherited visibility may still be hiding the glyph,
                        // but the animation will play anyway.
                        commands.entity(entity).insert(Appeared::default());
                    }
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
    spans_entities: Query<(&Glyph, &SpanGlyphOf)>,
) -> Result {
    let mut span_indices = EntityHashMap::<Vec<_>>::default();
    for (glyphs, block) in blocks.iter() {
        for entity in glyphs.iter() {
            let (glyph, span_of) = spans_entities.get(entity)?;
            span_indices.entry(span_of.entity()).or_default().push((
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

fn typewriter(
    mut commands: Commands,
    time: Res<Time>,
    mut typewriters: Query<(
        Entity,
        &Glyphs,
        &ComputedTextBlock,
        &TypewriterMode,
        &mut Typewriter,
        Mut<Reveal>,
        Option<&mut PauseTypewriter>,
        Option<&Children>,
        Has<DisableCommands>,
    )>,
    mut writer: EventWriter<TypewriterEvent>,
    glyph_query: Query<&Glyph>,
    spans: Query<&ByteRange, With<TextSpan>>,
    effects: Query<&TypewriterCommand>,
    events: Query<&TypewriterEvent>,
    callbacks: Query<&TypewriterCallback>,
) -> Result {
    for (entity, glyphs, block, mode, mut tw, mut reveal, pause, children, commands_disabled) in
        typewriters.iter_mut()
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
                                TypewriterCommand::Pause(dur) => {
                                    commands
                                        .entity(entity)
                                        .insert(PauseTypewriter::from_seconds(dur));
                                }
                                TypewriterCommand::Speed(mult) => {
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
                        let event = TypewriterEvent(event.0.clone());
                        writer.write(event.clone());
                        commands.entity(entity).trigger(event.clone());
                    }
                }
            }

            commands
                .entity(entity)
                .remove::<(Typewriter, TypewriterMode, Reveal)>()
                .trigger(TypewriterFinished);
            continue;
        }

        if let Some(mut pause) = pause {
            pause.0.tick(time.delta());
            if pause.0.finished() {
                commands.entity(entity).remove::<PauseTypewriter>();
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
                            TypewriterCommand::Pause(dur) => {
                                commands
                                    .entity(entity)
                                    .insert(PauseTypewriter::from_seconds(dur));
                                should_pause = true;
                                break;
                            }
                            TypewriterCommand::Speed(mult) => {
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
                    let event = TypewriterEvent(event.0.clone());
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
                .remove::<(Typewriter, TypewriterMode, Reveal)>()
                .trigger(TypewriterFinished);
            continue;
        }

        tw.timer.tick(time.delta());
        if tw.timer.just_finished() {
            match mode {
                TypewriterMode::Glyph => {
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
                TypewriterMode::Word => {
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
