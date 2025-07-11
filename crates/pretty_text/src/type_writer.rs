use std::ops::Range;
use std::time::Duration;

use bevy::ecs::entity::EntityHashMap;
use bevy::prelude::*;
use bevy::text::ComputedTextBlock;

use crate::PrettyText;
use crate::glyph::{Glyph, GlyphOf, GlyphSpanEntity, Glyphs};

pub struct TypeWriterPlugin;

impl Plugin for TypeWriterPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<GlyphRevealed>()
            .add_event::<TypeWriterFinished>()
            .add_event::<TypeWriterEvent>()
            .register_type::<TypeWriterEffect>()
            .add_systems(
                FixedUpdate,
                (calculate_byte_range, type_writer, reveal_glyphs).chain(),
            )
            .add_observer(removed_reveal);

        app.register_type::<TypeWriter>()
            .register_type::<TypeWriterMode>()
            .register_type::<TypeWriterFinished>()
            .register_type::<GlyphRevealed>()
            .register_type::<PauseTypeWriter>()
            .register_type::<Reveal>()
            .register_type::<TypeWriterEffect>()
            .register_type::<TypeWriterEvent>();
    }
}

#[derive(Debug, Clone, Component, Reflect)]
#[require(PrettyText, TypeWriterMode, Reveal)]
pub struct TypeWriter {
    speed: f32,
    timer: Timer,
    processed_children: Vec<Entity>,
}

impl TypeWriter {
    pub fn new(speed: f32) -> Self {
        let dur = 1.0 / speed;
        let mut timer = Timer::from_seconds(dur, TimerMode::Repeating);
        timer.set_elapsed(Duration::from_secs_f32(dur));

        Self {
            speed,
            timer,
            processed_children: Vec::new(),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, Component, Reflect)]
pub enum TypeWriterMode {
    #[default]
    Glpyh,
    Word,
}

#[derive(Debug, Clone, Event, Reflect)]
pub struct GlyphRevealed {
    pub glyph: Option<Entity>,
    pub text: String,
}

#[derive(Debug, Clone, Event, Reflect)]
pub struct WordRevealed {
    pub glyphs: Vec<Entity>,
    pub text: String,
}

#[derive(Debug, Clone, Copy, Event, Reflect)]
pub struct TypeWriterFinished;

#[derive(Debug, Clone, Component, Reflect)]
pub struct PauseTypeWriter(pub Timer);

impl PauseTypeWriter {
    pub fn from_seconds(duration: f32) -> Self {
        Self(Timer::from_seconds(duration, TimerMode::Once))
    }
}

#[derive(Debug, Default, Clone, Copy, Component, Reflect)]
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

#[derive(Debug, Clone, Component, Event, Reflect)]
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
    reveal: Query<
        (&Glyphs, &ComputedTextBlock, &Reveal),
        Or<(Changed<Reveal>, Added<Reveal>, Changed<Glyphs>)>,
    >,
    mut visibilities: Query<(&mut Visibility, &Glyph), With<GlyphOf>>,
) {
    for (glyphs, block, reveal) in reveal.iter() {
        for entity in glyphs.iter() {
            if let Ok((mut vis, glyph)) = visibilities.get_mut(entity) {
                let line_offset = block
                    .buffer()
                    .lines
                    .iter()
                    .take(glyph.0.line_index)
                    .map(|line| line.text().len())
                    .sum::<usize>();

                let target = if line_offset + glyph.0.byte_index + glyph.0.byte_length <= reveal.0 {
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
                    .map(|line| line.text().len())
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
                    .map(|line| line.text().len())
                    .sum::<usize>()
                    + *end
            })
            .max()
            .unwrap();
        commands.entity(span_entity).insert(ByteRange(start..end));
    }

    Ok(())
}

// TODO: The type writer reveals codepoint-by-codepoint and not glyph-by-glyph because some of the
// glyphs are stripped by the layout system.
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
    )>,
    mut writer: EventWriter<TypeWriterEvent>,
    glyph_query: Query<&Glyph>,
    spans: Query<&ByteRange, With<TextSpan>>,
    effects: Query<&TypeWriterEffect>,
    events: Query<&TypeWriterEvent>,
) -> Result {
    for (entity, glyphs, block, mode, mut tw, mut reveal, pause, children) in
        type_writers.iter_mut()
    {
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

                if let Ok(event) = events.get(child) {
                    tw.processed_children.push(child);
                    writer.write(event.clone());
                    commands.entity(entity).trigger(event.clone());
                    continue;
                }

                if let Ok(effect) = effects.get(child) {
                    tw.processed_children.push(child);
                    match *effect {
                        TypeWriterEffect::Pause(dur) => {
                            commands
                                .entity(entity)
                                .insert(PauseTypeWriter::from_seconds(dur));
                            should_pause = true;
                            break;
                        }
                        TypeWriterEffect::Speed(mult) => {
                            let speed = tw.speed;
                            tw.timer
                                .set_duration(Duration::from_secs_f32(1. / speed / mult));
                        }
                    }
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
                accum += line.text().len();
                let take = accum <= reveal.0;
                if take {
                    line_offset += line.text().len();
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
                TypeWriterMode::Glpyh => {
                    let text = block
                        .buffer()
                        .lines
                        .get(line_index)
                        .map(|line| {
                            &line.text()[reveal.0 - line_offset..reveal.0 - line_offset + 1]
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
                        text: text.to_string(),
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
                        .unwrap_or(word_slice.len());

                    let end = start + word_end_offset;
                    let mut last_char = text[start..start + 1].chars().next().unwrap();

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
