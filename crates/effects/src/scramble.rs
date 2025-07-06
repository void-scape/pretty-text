use std::ops::Range;
use std::time::Duration;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::text::{ComputedTextBlock, FontSmoothing, PositionedGlyph, TextLayoutInfo, Update2dText};
use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
use pretty_text::glyph::{Glyph, GlyphSpanEntity};
use pretty_text::{PrettyText, access::GlyphReader};
use pretty_text_macros::TextEffect;
use rand::Rng;

pub struct ScramblePlugin;

impl Plugin for ScramblePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<LayoutCache>()
            .add_systems(
                PostUpdate,
                (
                    insert_scramble.before(Update2dText),
                    scramble_glyph.after(Update2dText),
                ),
            )
            .register_pretty_effect::<DynamicScramble>("scrambled");

        app.register_type::<Scramble>()
            .register_type::<ScrambleLifetime>()
            .register_type::<ScrambleSpeed>();
    }
}

#[derive(Bundle, Default, TextEffect)]
#[pretty_text_path(pretty_text)]
pub struct DynamicScramble {
    #[effect(skip)]
    pub scramble: Scramble,
    pub speed: ScrambleSpeed,
    pub lifetime: ScrambleLifetime,
}

#[derive(Debug, Default, Component, Reflect)]
#[require(PrettyText, ScrambleSpeed, ScrambleLifetime)]
pub struct Scramble;

#[derive(Debug, Clone, Component, Reflect)]
pub enum ScrambleSpeed {
    Fixed(f32),
    Random(Range<f32>),
}

impl Default for ScrambleSpeed {
    fn default() -> Self {
        Self::Fixed(12f32)
    }
}

impl std::str::FromStr for ScrambleSpeed {
    type Err = <f32 as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Self::Fixed(s.parse()?))
    }
}

#[derive(Debug, Clone, Component, Reflect)]
pub enum ScrambleLifetime {
    Fixed(f32),
    Random(Range<f32>),
}

impl Default for ScrambleLifetime {
    fn default() -> Self {
        ScrambleLifetime::Fixed(0.5)
    }
}

impl std::str::FromStr for ScrambleLifetime {
    type Err = <f32 as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Self::Fixed(s.parse()?))
    }
}

// TODO: clean this guy up
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
    mut commands: Commands,
    mut cache: ResMut<LayoutCache>,
    scramble: Query<(&ScrambleSpeed, &ScrambleLifetime)>,
    glyphs: Query<
        (Entity, &Glyph, &GlyphSpanEntity, Option<&UnscrambledGlyph>),
        Or<(Changed<Visibility>, Added<Visibility>)>,
    >,
    style: Query<(&TextFont, &TextColor)>,
    reader: GlyphReader,
) -> Result {
    for (entity, glyph, span_entity, unscrambled) in glyphs.iter() {
        let Ok((root_speed, root_lifetime)) = scramble.get(span_entity.0) else {
            continue;
        };

        if reader.read(entity)? == " " {
            continue;
        }

        let mut next_scramble = NextScramble(Timer::from_seconds(0.0, TimerMode::Repeating));
        let mut lifetime = Lifetime::default();
        set_timers(
            &mut rand::rng(),
            root_speed,
            root_lifetime,
            &mut next_scramble,
            &mut lifetime,
        );
        next_scramble.0.set_elapsed(next_scramble.0.duration());

        let (font, color) = style.get(span_entity.0)?;
        let layout_entity = *cache
            .0
            .entry(LayoutHash::new(font, color))
            .or_insert_with(|| {
                // TODO: clean this guy up
                commands
                    .spawn((
                        Visibility::Hidden,
                        Text2d::new("abcdefghijklmnopqrstuvwxyz0123456789"),
                        font.clone(),
                        *color,
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
    for (
        entity,
        mut glyph,
        mut lifetime,
        mut next_scramble,
        layout_entity,
        unscrambled,
        span_entity,
    ) in glyphs.iter_mut()
    {
        if let Ok((root_lifetime, root_speed)) = scramble_config.get(span_entity.0) {
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
            let (layout, computed) = layouts.get(layout_entity.0)?;

            // this should basically never happen, and if it does then there is a bug in
            // the scamble code in which case stalling here would be annoying
            let max_depth = 10;
            let mut depth = 0;
            let mut new_glyph;
            loop {
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
