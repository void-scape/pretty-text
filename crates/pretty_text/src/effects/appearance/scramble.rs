use std::ops::Range;
use std::time::Duration;

use bevy::prelude::*;
use bevy::text::{ComputedTextBlock, PositionedGlyph, TextLayoutInfo};
use pretty_text_macros::{DynamicEffect, parser_syntax};
use rand::Rng;
use rand::rngs::ThreadRng;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_spans};
use crate::glyph::{Glyph, SpanGlyphOf};
use crate::parser::{ArgParser, duration_secs, range, tuple_struct};
use crate::style::PrettyStyleSet;

use super::Appeared;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (
            mark_effect_spans::<Scramble, ScrambleSpan>,
            (scramble_span, scramble_appeared_glyph),
            scramble,
        )
            .chain()
            .in_set(PrettyEffectSet),
    )
    .add_systems(PostUpdate, scramble_appeared_glyph.after(PrettyStyleSet))
    .register_pretty_effect::<Scramble>("scramble");

    app.register_type::<Scramble>()
        .register_type::<ScrambleLifetime>()
        .register_type::<ScrambleSpeed>();
}

/// Cycles between random, alphanumeric glyphs.
#[derive(Debug, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[parser_syntax]
pub struct Scramble {
    /// Controls the time in seconds that a glyph is retained.
    #[syntax(
        default = ScrambleSpeed::Fixed(12.0) => "12",
        "duration" => "fixed(duration)",
        "start..end" => "random(start..end)",
    )]
    pub speed: ScrambleSpeed,

    /// Controls the time in seconds that a glyph will scramble.
    #[syntax(
        default = ScrambleLifetime::Fixed(0.5) => "0.5",
        "always",
        "duration" => "fixed(duration)",
        "start..end" => "random(start..end)",
    )]
    pub lifetime: ScrambleLifetime,
}

/// Controls the time in seconds that a glyph is retained.
///
/// After this time has elapsed, a new glyph will be chosen.
///
/// See [`Scramble`].
#[derive(Debug, Clone, Reflect, serde::Deserialize)]
pub enum ScrambleSpeed {
    /// Fixed duration (in seconds).
    Fixed(f32),

    /// Randomly chosen duration (in seconds).
    ///
    /// Chooses a new value after the current glyph is scrambled.
    Random(Range<f32>),
}

impl ArgParser for ScrambleSpeed {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        use winnow::Parser;
        use winnow::combinator::*;

        winnow::combinator::alt((
            preceded(
                "fixed",
                delimited("(", duration_secs.map(ScrambleSpeed::Fixed), ")"),
            ),
            preceded(
                "random",
                delimited("(", range.map(ScrambleSpeed::Random), ")"),
            ),
            range.map(ScrambleSpeed::Random),
            duration_secs.map(ScrambleSpeed::Fixed),
        ))
        .parse_next(input)
    }
}

/// Controls the time in seconds that a glyph will scramble.
///
/// After this time has elapsed, the original glyph will be assigned.
///
/// See [`Scramble`].
#[derive(Debug, Clone, Reflect, serde::Deserialize)]
pub enum ScrambleLifetime {
    /// Persistent scrambling.
    Always,

    /// Fixed duration (in seconds).
    Fixed(f32),

    /// Randomly chosen duration (in seconds).
    Random(Range<f32>),
}

impl ArgParser for ScrambleLifetime {
    fn parse_arg(input: &mut &str) -> winnow::ModalResult<Self> {
        use winnow::Parser;

        winnow::combinator::alt((
            "always".map(|_| ScrambleLifetime::Always),
            tuple_struct("fixed", duration_secs.map(ScrambleLifetime::Fixed)),
            tuple_struct("random", range.map(ScrambleLifetime::Random)),
            range.map(ScrambleLifetime::Random),
            duration_secs.map(ScrambleLifetime::Fixed),
        ))
        .parse_next(input)
    }
}

// Marker for spans with the scramble effect
#[derive(Default, Component)]
struct ScrambleSpan;

// Points to a hidden text entity whose layout will be stolen (͡°͜ʖ͡°)
#[derive(Clone, Copy, Component)]
struct DummyLayout(Entity);

// The glyph that was original pointed to
#[derive(Component)]
struct UnscrambledGlyph(PositionedGlyph);

#[derive(Default, Component)]
struct Lifetime(Option<Timer>);

#[derive(Component)]
struct NextScramble(Timer);

fn scramble_span(
    mut commands: Commands,
    mut spans: Query<
        (Entity, &TextFont, Option<&mut DummyLayout>),
        (
            With<ScrambleSpan>,
            Or<(Added<ScrambleSpan>, Changed<TextFont>)>,
        ),
    >,
) {
    for (entity, font, dummy_layout) in spans.iter_mut() {
        let dummy = commands
            .spawn((
                ChildOf(entity),
                Visibility::Hidden,
                Text2d::new("abcdefghijklmnopqrstuvwxyz0123456789"),
                font.clone(),
            ))
            .id();
        match dummy_layout {
            Some(mut dummy_layout) => dummy_layout.0 = dummy,
            None => {
                commands.entity(entity).insert(DummyLayout(dummy));
            }
        }
    }
}

fn scramble_appeared_glyph(
    mut commands: Commands,
    scramble: EffectQuery<&Scramble>,
    glyphs: Query<(Entity, &Glyph, &SpanGlyphOf), (Added<Appeared>, Without<UnscrambledGlyph>)>,
    layout: Query<&DummyLayout>,
) -> Result {
    for (entity, glyph, span) in glyphs.iter() {
        let Some(scramble) = scramble.iter(span.entity()).next() else {
            continue;
        };

        let mut next_scramble = NextScramble(Timer::from_seconds(0.0, TimerMode::Repeating));
        let mut lifetime = Lifetime::default();
        set_timers(
            &mut rand::rng(),
            &scramble.speed,
            &scramble.lifetime,
            &mut next_scramble,
            &mut lifetime,
        );
        next_scramble.0.set_elapsed(next_scramble.0.duration());

        let layout = layout.get(span.entity())?;
        commands.entity(entity).insert((
            UnscrambledGlyph(glyph.0.clone()),
            *layout,
            next_scramble,
            lifetime,
        ));
    }

    Ok(())
}

fn scramble(
    mut commands: Commands,
    time: Res<Time>,
    scramble_config: EffectQuery<&Scramble, Changed<Scramble>>,
    layouts: Query<(&TextLayoutInfo, &ComputedTextBlock)>,
    mut glyphs: Query<(
        Entity,
        &mut Glyph,
        &mut Lifetime,
        &mut NextScramble,
        &DummyLayout,
        &UnscrambledGlyph,
        &SpanGlyphOf,
    )>,
) -> Result {
    if glyphs.is_empty() {
        return Ok(());
    }

    let mut rng = rand::rng();
    'outer: for (
        entity,
        mut glyph,
        mut lifetime,
        mut next_scramble,
        layout_entity,
        unscrambled,
        span_entity,
    ) in glyphs.iter_mut()
    {
        if let Some(scramble) = scramble_config.iter(span_entity.entity()).next() {
            set_timers(
                &mut rng,
                &scramble.speed,
                &scramble.lifetime,
                &mut next_scramble,
                &mut lifetime,
            );
        }

        if let Some(lifetime) = lifetime.0.as_mut() {
            lifetime.tick(time.delta());
            if lifetime.finished() {
                commands
                    .entity(entity)
                    .remove::<(DummyLayout, UnscrambledGlyph, Lifetime, NextScramble)>();
                glyph.0 = unscrambled.0.clone();
                continue;
            }
        }

        next_scramble.0.tick(time.delta());
        if next_scramble.0.just_finished() {
            let (layout, computed) = layouts.get(layout_entity.0)?;

            // this should basically never happen, and if it does then there is a bug in
            // the scamble code in which case stalling here would be annoying
            let max_depth = 10;
            let mut depth = 0;
            let mut new_glyph;
            loop {
                if layout.glyphs.is_empty() {
                    continue 'outer;
                }

                new_glyph = &layout.glyphs[rng.random_range(0..layout.glyphs.len())];
                let str = computed.buffer().lines[new_glyph.line_index].text();
                if &str[new_glyph.byte_index..new_glyph.byte_index + new_glyph.byte_length] != " " {
                    break;
                }

                if depth >= max_depth {
                    break;
                }

                depth += 1;
            }

            glyph.0.atlas_info = new_glyph.atlas_info.clone();
            glyph.0.span_index = new_glyph.span_index;
            glyph.0.size = new_glyph.size;
        }
    }

    Ok(())
}

fn set_timers(
    rng: &mut ThreadRng,
    root_speed: &ScrambleSpeed,
    root_lifetime: &ScrambleLifetime,
    next_scramble: &mut NextScramble,
    lifetime: &mut Lifetime,
) {
    match root_speed {
        ScrambleSpeed::Fixed(speed) => {
            next_scramble
                .0
                .set_duration(Duration::from_secs_f32(1. / *speed));
        }
        ScrambleSpeed::Random(range) => {
            next_scramble.0.set_duration(Duration::from_secs_f32(
                1. / rng.random_range(range.clone()),
            ));
        }
    }

    match root_lifetime {
        ScrambleLifetime::Always => {
            lifetime.0 = None;
        }
        ScrambleLifetime::Fixed(duration) => {
            lifetime.0 = Some(Timer::from_seconds(*duration, TimerMode::Repeating));
        }
        ScrambleLifetime::Random(range) => {
            lifetime.0 = Some(Timer::from_seconds(
                rng.random_range(range.clone()),
                TimerMode::Repeating,
            ));
        }
    }
}
