use std::ops::Range;

use bevy::ecs::entity::EntityHashMap;
use bevy::prelude::*;
use bevy::text::{ComputedTextBlock, PositionedGlyph, TextLayoutInfo};
use pretty_text_macros::{DynamicEffect, parser_syntax};
use rand::Rng;

use crate::PrettyText;
use crate::effects::dynamic::PrettyTextEffectAppExt;
use crate::effects::{EffectQuery, PrettyEffectSet, mark_effect_glyphs};
use crate::glyph::{Glyph, SpanGlyphOf};
use crate::parser::{ArgParser, duration_secs, range, tuple_struct};

use super::Appeared;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (mark_effect_glyphs::<Scramble, ComputeScramble>, scramble)
            .chain()
            .in_set(PrettyEffectSet),
    )
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

#[derive(Default, Component)]
struct ComputeScramble;

#[derive(Component)]
struct CachedGlyph(PositionedGlyph);

#[derive(Component)]
struct Lifetime(f32);

#[derive(Component)]
struct ScrambleTimer(Timer);

#[derive(Component)]
struct ScrambleLayout(Entity);

#[derive(Component)]
struct ScrambleLayoutEntity;

fn scramble(
    mut commands: Commands,
    time: Res<Time>,
    scramble: EffectQuery<Ref<Scramble>>,
    mut glyphs: Query<
        (
            Entity,
            &mut Glyph,
            &SpanGlyphOf,
            Option<&CachedGlyph>,
            Option<&Lifetime>,
            Option<&mut ScrambleTimer>,
            Option<&ScrambleLayout>,
            &Appeared,
            &mut Visibility,
        ),
        With<ComputeScramble>,
    >,
    layouts: Query<(&TextLayoutInfo, &ComputedTextBlock)>,
    fonts: Query<&TextFont>,
    mut layout_hash: Local<EntityHashMap<Entity>>,
) -> Result {
    let mut rng = rand::rng();
    'outer: for (
        entity,
        mut glyph,
        span_entity,
        cached,
        lifetime,
        mut timer,
        scramble_layout,
        appeared,
        mut visibility,
    ) in glyphs.iter_mut()
    {
        let Ok(scramble) = scramble.get(span_entity) else {
            continue;
        };

        type ScrambleComponents = (CachedGlyph, ScrambleTimer, ScrambleLayout);

        if let Some(lifetime) = lifetime {
            if lifetime.0 <= appeared.0 {
                if let Some(cached) = cached {
                    glyph.0 = cached.0.clone();
                }
                commands.entity(entity).remove::<ScrambleComponents>();
                continue;
            }
        }

        let new_duration = scramble.is_changed() || lifetime.is_none();
        let duration = match &scramble.lifetime {
            ScrambleLifetime::Always => None,
            ScrambleLifetime::Fixed(duration) => new_duration.then_some(*duration),
            ScrambleLifetime::Random(range) => {
                new_duration.then(|| rng.random_range(range.clone()))
            }
        };

        if let Some(duration) = duration {
            if duration <= 0.0 {
                if let Some(cached) = cached {
                    glyph.0 = cached.0.clone();
                }
                commands.entity(entity).remove::<ScrambleComponents>();
                continue;
            }
            commands.entity(entity).insert(Lifetime(duration));
        }

        let new_speed = scramble.is_changed() || timer.is_none();
        let speed = match &scramble.speed {
            ScrambleSpeed::Fixed(speed) => new_speed.then_some(*speed),
            ScrambleSpeed::Random(range) => new_speed.then(|| rng.random_range(range.clone())),
        };

        if cached.is_none() {
            commands.entity(entity).insert(CachedGlyph(glyph.0.clone()));
        }

        if scramble_layout.is_none() {
            let layout_entity = layout_hash.entry(span_entity.0).or_insert_with(|| {
                let font = fonts
                    .get(span_entity.0)
                    .expect("text span has no `TextFont`");
                commands
                    .spawn((
                        ScrambleLayoutEntity,
                        Visibility::Hidden,
                        Text2d::new("abcdefghijklmnopqrstuvwxyz0123456789"),
                        font.clone(),
                    ))
                    .id()
            });

            commands
                .entity(entity)
                .insert(ScrambleLayout(*layout_entity));
            // Hide glyph until the layout is ready.
            if *visibility != Visibility::Hidden {
                *visibility = Visibility::Hidden;
            }
        }

        if let Some(speed) = speed {
            if speed <= 0.0 {
                if let Some(cached) = cached {
                    glyph.0 = cached.0.clone();
                }
                commands.entity(entity).remove::<ScrambleComponents>();
                continue;
            }
            commands
                .entity(entity)
                .insert(ScrambleTimer(Timer::from_seconds(
                    1.0 / speed,
                    TimerMode::Repeating,
                )));
            continue;
        }

        let timer = timer.as_mut().unwrap();
        timer.0.tick(time.delta());
        if timer.0.just_finished() {
            let Some(scramble_layout) = scramble_layout else {
                continue;
            };
            let (layout, computed) = layouts.get(scramble_layout.0)?;

            // Reveal glyph now that layout is ready.
            if *visibility != Visibility::Inherited {
                *visibility = Visibility::Inherited;
            }

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
