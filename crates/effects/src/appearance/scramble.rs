use std::ops::Range;
use std::time::Duration;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::text::{ComputedTextBlock, FontSmoothing, PositionedGlyph, TextLayoutInfo};
use bevy_pretty_text::glyph::GlyphSystems;
use bevy_pretty_text::parser::{ArgParser, duration_secs, range};
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{Glyph, GlyphSpanEntity};
use pretty_text::{PrettyText, access::GlyphReader};
use pretty_text_macros::{DynamicEffect, dynamic_effect_docs};
use rand::Rng;
use rand::rngs::ThreadRng;

use crate::PrettyEffectSet;

pub(super) fn plugin(app: &mut App) {
    app.init_resource::<LayoutCache>()
        .add_systems(
            Update,
            scramble_glyph
                .in_set(ScrambleSystems::Update)
                .in_set(PrettyEffectSet),
        )
        .add_systems(
            PostUpdate,
            insert_scramble
                .in_set(ScrambleSystems::Init)
                .after(GlyphSystems::Construct),
        )
        .register_pretty_effect::<Scramble>("scramble");

    app.register_type::<Scramble>()
        .register_type::<ScrambleLifetime>()
        .register_type::<ScrambleSpeed>();
}

/// A [`SystemSet`] for [`Scramble`] systems.
#[derive(Debug, Clone, Copy, SystemSet, Eq, PartialEq, Hash)]
pub enum ScrambleSystems {
    /// Initializes revealed [`Glyph`]s with the [`Scramble`] effect.
    ///
    /// Runs in the [`PostUpdate`] schedule after [`GlyphSystems::Construct`].
    Init,

    /// Scrambles [`Glyph`]s, removing constituent components when finished.
    ///
    /// Runs in the [`Update`] schedule in the [`PrettyEffectSet`].
    Update,
}

/// Cycles between random, alphanumeric glyphs.
#[derive(Debug, Component, Reflect, DynamicEffect)]
#[require(PrettyText)]
#[dynamic_effect_docs]
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
        use winnow::combinator::*;

        winnow::combinator::alt((
            "always".map(|_| ScrambleLifetime::Always),
            preceded(
                "fixed",
                delimited("(", duration_secs.map(ScrambleLifetime::Fixed), ")"),
            ),
            preceded(
                "random",
                delimited("(", range.map(ScrambleLifetime::Random), ")"),
            ),
            range.map(ScrambleLifetime::Random),
            duration_secs.map(ScrambleLifetime::Fixed),
        ))
        .parse_next(input)
    }
}

// TODO: clean this guy up
#[derive(Default, Resource)]
struct LayoutCache(HashMap<LayoutHash, Entity>);

#[derive(PartialEq, Eq, Hash)]
struct LayoutHash {
    font: AssetId<Font>,
    font_size: i32,
    line_height: LineHeightHash,
    font_smoothing: FontSmoothing,
}

impl LayoutHash {
    pub fn new(font: &TextFont) -> Self {
        Self {
            font: font.font.id(),
            font_size: (font.font_size * 1_000f32) as i32,
            line_height: match font.line_height {
                bevy::text::LineHeight::Px(px) => LineHeightHash::Px((px * 1_000f32) as i32),
                bevy::text::LineHeight::RelativeToFont(rel) => {
                    LineHeightHash::RelativeToFont((rel * 1_000f32) as i32)
                }
            },
            font_smoothing: font.font_smoothing,
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
enum LineHeightHash {
    Px(i32),
    RelativeToFont(i32),
}

fn insert_scramble(
    mut commands: Commands,
    mut cache: ResMut<LayoutCache>,
    scramble: Query<&Scramble>,
    glyphs: Query<
        (Entity, &Glyph, &GlyphSpanEntity, Option<&UnscrambledGlyph>),
        Or<(Changed<Visibility>, Added<Visibility>)>,
    >,
    style: Query<&TextFont>,
    reader: GlyphReader,
) -> Result {
    for (entity, glyph, span_entity, unscrambled) in glyphs.iter() {
        let Ok(scramble) = scramble.get(span_entity.0) else {
            continue;
        };

        if reader.read(entity)? == " " {
            continue;
        }

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

        let font = style.get(span_entity.0)?;
        let layout_entity = *cache.0.entry(LayoutHash::new(font)).or_insert_with(|| {
            // stops spurious warnings
            commands
                .entity(span_entity.0)
                .insert((Transform::default(), Visibility::Inherited));

            commands
                .spawn((
                    ChildOf(span_entity.0),
                    Visibility::Hidden,
                    Text2d::new("abcdefghijklmnopqrstuvwxyz0123456789"),
                    font.clone(),
                ))
                .id()
        });

        if unscrambled.is_none() {
            commands.entity(entity).insert((
                UnscrambledGlyph(glyph.0.clone()),
                LayoutEntity(layout_entity),
                next_scramble,
                lifetime,
            ));
        } else {
            commands
                .entity(entity)
                .insert((LayoutEntity(layout_entity), next_scramble, lifetime));
        }
    }

    Ok(())
}

#[derive(Component)]
struct LayoutEntity(Entity);

#[derive(Component)]
struct UnscrambledGlyph(PositionedGlyph);

#[derive(Default, Component)]
struct Lifetime(Option<Timer>);

#[derive(Component)]
struct NextScramble(Timer);

fn scramble_glyph(
    mut commands: Commands,
    time: Res<Time>,
    scramble_config: Query<&Scramble, Changed<Scramble>>,
    layouts: Query<(&TextLayoutInfo, &ComputedTextBlock)>,
    mut glyphs: Query<(
        Entity,
        &mut Glyph,
        &mut Lifetime,
        &mut NextScramble,
        &LayoutEntity,
        &UnscrambledGlyph,
        &GlyphSpanEntity,
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
        if let Ok(scramble) = scramble_config.get(span_entity.0) {
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
                    .remove::<(LayoutEntity, UnscrambledGlyph, Lifetime, NextScramble)>();
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
