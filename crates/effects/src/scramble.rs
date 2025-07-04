use std::ops::Range;
use std::time::Duration;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::text::{FontSmoothing, PositionedGlyph, TextLayoutInfo, Update2dText};
use pretty_text::PrettyText;
use pretty_text::access::GlyphReader;
use pretty_text::glyph::{Glyph, GlyphOf, GlyphSpanEntity};
use rand::Rng;

pub struct ScramblePlugin;

impl Plugin for ScramblePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<LayoutCache>().add_systems(
            PostUpdate,
            (
                insert_scramble.before(Update2dText),
                scramble_glyph.after(Update2dText),
            ),
        );
    }
}

#[derive(Component)]
#[require(PrettyText, ScrambleSpeed, ScrambleLifetime)]
pub struct Scramble;

#[derive(Component)]
pub enum ScrambleSpeed {
    Fixed(f32),
    Random(Range<f32>),
}

impl Default for ScrambleSpeed {
    fn default() -> Self {
        Self::Fixed(12f32)
    }
}

#[derive(Component)]
pub enum ScrambleLifetime {
    Fixed(f32),
    Random(Range<f32>),
}

impl Default for ScrambleLifetime {
    fn default() -> Self {
        ScrambleLifetime::Fixed(0.5)
    }
}

#[derive(Default, Resource)]
struct LayoutCache(HashMap<LayoutHash, Entity>);

#[derive(PartialEq, Eq, Hash)]
struct LayoutHash {
    font: TextFontHash,
    color: [u8; 4],
}

impl LayoutHash {
    pub fn new(font: &TextFont, color: &TextColor) -> Self {
        Self {
            font: TextFontHash {
                font: font.font.id(),
                font_size: (font.font_size * 1_000f32) as i32,
                line_height: match font.line_height {
                    bevy::text::LineHeight::Px(px) => LineHeightHash::Px((px * 1_000f32) as i32),
                    bevy::text::LineHeight::RelativeToFont(rel) => {
                        LineHeightHash::RelativeToFont((rel * 1_000f32) as i32)
                    }
                },
                font_smoothing: font.font_smoothing,
            },
            color: color.0.to_linear().to_u8_array(),
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
struct TextFontHash {
    font: AssetId<Font>,
    font_size: i32,
    line_height: LineHeightHash,
    font_smoothing: FontSmoothing,
}

#[derive(PartialEq, Eq, Hash)]
enum LineHeightHash {
    Px(i32),
    RelativeToFont(i32),
}

fn insert_scramble(
    scramble: Query<(&ScrambleSpeed, &ScrambleLifetime)>,
    mut commands: Commands,
    glyphs: Query<
        (Entity, &Glyph, &Visibility, &GlyphOf, &GlyphSpanEntity),
        Or<(Changed<Visibility>, Added<Visibility>)>,
    >,
    style: Query<(&TextFont, &TextColor)>,
    mut cache: ResMut<LayoutCache>,
    mut reader: GlyphReader,
) -> Result {
    for (entity, glyph, vis, glyph_of, span_entity) in glyphs.iter() {
        if *vis == Visibility::Hidden {
            continue;
        }

        if reader.read(entity)? == " " {
            continue;
        }

        let Ok((root_speed, root_lifetime)) = scramble.get(glyph_of.0) else {
            continue;
        };

        let mut next_scramble = NextScramble(Timer::from_seconds(0.0, TimerMode::Repeating));
        let mut lifetime = Lifetime::default();
        set_timers(
            &mut rand::rng(),
            root_speed,
            root_lifetime,
            &mut next_scramble,
            &mut lifetime,
        );

        let (font, color) = style.get(span_entity.0)?;
        let layout_entity = *cache
            .0
            .entry(LayoutHash::new(font, color))
            .or_insert_with(|| {
                commands
                    .spawn((
                        Visibility::Hidden,
                        Text2d::new("abcdefghijklmnopqrstuvwxyz0123456789"),
                        font.clone(),
                        *color,
                    ))
                    .id()
            });

        commands.entity(entity).insert((
            UnscrambledGlyph(glyph.0.clone()),
            LayoutEntity(layout_entity),
            next_scramble,
            lifetime,
        ));
    }

    Ok(())
}

#[derive(Component)]
struct LayoutEntity(Entity);

#[derive(Component)]
struct UnscrambledGlyph(PositionedGlyph);

#[derive(Default, Component)]
struct Lifetime(Timer);

#[derive(Component)]
struct NextScramble(Timer);

fn scramble_glyph(
    mut commands: Commands,
    time: Res<Time>,
    scramble_config: Query<
        (&ScrambleLifetime, &ScrambleSpeed),
        Or<(Changed<ScrambleLifetime>, Changed<ScrambleSpeed>)>,
    >,
    layouts: Query<&TextLayoutInfo>,
    mut glyphs: Query<(
        Entity,
        &mut Glyph,
        &mut Lifetime,
        &mut NextScramble,
        &LayoutEntity,
        &UnscrambledGlyph,
        &GlyphOf,
    )>,
) -> Result {
    if glyphs.is_empty() {
        return Ok(());
    }

    let mut rng = rand::rng();
    for (
        entity,
        mut glyph,
        mut lifetime,
        mut next_scramble,
        layout_entity,
        unscrambled,
        glyph_of,
    ) in glyphs.iter_mut()
    {
        if let Ok((root_lifetime, root_speed)) = scramble_config.get(glyph_of.0) {
            set_timers(
                &mut rng,
                root_speed,
                root_lifetime,
                &mut next_scramble,
                &mut lifetime,
            );
        }

        lifetime.0.tick(time.delta());
        if lifetime.0.finished() {
            commands
                .entity(entity)
                .remove::<(LayoutEntity, UnscrambledGlyph, Lifetime, NextScramble)>();
            glyph.0 = unscrambled.0.clone();
            continue;
        }

        next_scramble.0.tick(time.delta());
        if next_scramble.0.just_finished() {
            let layout = layouts.get(layout_entity.0)?;
            let position = glyph.0.position;
            glyph.0 = layout.glyphs[rng.random_range(0..layout.glyphs.len())].clone();
            glyph.0.position = position;
        }
    }

    Ok(())
}

fn set_timers(
    rng: &mut impl Rng,
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
        ScrambleLifetime::Fixed(duration) => {
            lifetime.0.set_duration(Duration::from_secs_f32(*duration));
        }
        ScrambleLifetime::Random(range) => {
            lifetime
                .0
                .set_duration(Duration::from_secs_f32(rng.random_range(range.clone())));
        }
    }
}
