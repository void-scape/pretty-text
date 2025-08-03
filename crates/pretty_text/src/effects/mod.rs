//! Provides built-in text effects and APIs for writing custom effects.
//!
//! [See defining custom effects.](crate::effects::dynamic#defining-custom-effects)
//!
//! ## Categories
//!
//! The crate is split broadly into two categories: *behavior* and *appearance*.
//! Behavior effects apply to a glyph for its entire lifetime. Appearance effects
//! apply to a glyph when its visibility changes from hidden to visible.
//!
//! ## Representation
//!
//! Within these categories, there are two different representations of effects:
//! *ECS* and *material*. ECS effects update the position, scale, and rotation of
//! [`Glyph`] entities, whereas material effects set the [`GlyphMaterial`]. A
//! [`Glyph`] can _only have 1 material effect_. However, a [`Glyph`] can have
//! any number of ECS effects!
//!
//! [`GlyphMaterial`]: bevy_pretty_text::material::GlyphMaterial
//! [`Glyph`]: bevy_pretty_text::glyph::Glyph
//!
//! ### Behavior
//!
//! | Component | Tag | ECS Effect | Material Effect |
//! | --------- | --- | :--------: | :-------------: |
//! | [`Wave`] | `wave` | ✅ | ❌ |
//! | [`Shake`] | `shake` | ✅ | ❌ |
//! | [`Wobble`] | `wobble` | ✅ | ❌ |
//! | [`Glitch`] | `glitch` | ❌ | ✅ |
//! | [`Rainbow`] | `rainbow` | ❌ | ✅ |
//!
//! ### Appearance
//!
//! | Component | Tag | ECS Effect | Material Effect |
//! | --------- | --- | :--------: | :-------------: |
//! | [`Scramble`] | `scramble` | ✅ | ❌ |

use std::marker::PhantomData;

use bevy::ecs::query::{QueryData, QueryFilter};
use bevy::ecs::system::SystemParam;
use bevy::prelude::*;

use crate::glyph::{GlyphSpan, SpanGlyphs};
use crate::style::{StyleRegistry, Styles};

use appearance::Appeared;

pub mod appearance;
pub mod behavior;
pub mod dynamic;
pub mod material;

/// A [`SystemSet`] for all effect systems.
///
/// Runs in the [`Update`] schedule.
#[derive(Debug, Clone, Copy, SystemSet, Eq, PartialEq, Hash)]
pub struct PrettyEffectSet;

/// Initializes the built-in text effects for `bevy_pretty_text`.
#[derive(Debug)]
pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        behavior::plugin(app);
        appearance::plugin(app);
        material::plugin(app);
        app.init_resource::<dynamic::DynEffectRegistry>();
    }
}

#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = EffectOf, linked_spawn)]
pub struct Effects(Vec<Entity>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[relationship(relationship_target = Effects)]
pub struct EffectOf(pub Entity);

#[macro_export]
macro_rules! effects {
    [$($child:expr),*$(,)?] => {
       $crate::effects::Effects::spawn(($(::bevy::ecs::spawn::Spawn($child)),*))
    };
}

/// This system runs whenever a span's [`Styles`] changes. It checks if the span
/// points to an `Effect`. If it does, then `Marker` is inserted into the [`Glyph`],
/// otherwise it is removed.
///
/// [`Glyph`]: pretty_text::glyph::Glyph
pub fn mark_effect_glyphs<Effect: Component, Marker: Component + Default>(
    mut commands: Commands,
    effect: EffectQuery<&Effect>,
    styles: Query<
        (Entity, &SpanGlyphs),
        (With<Styles>, Or<(Changed<Styles>, Changed<SpanGlyphs>)>),
    >,
    marked_glyphs: Query<Entity, With<Marker>>,
    unmarked_glyphs: Query<Entity, Without<Marker>>,
) {
    for (span, glyphs) in styles.iter() {
        if effect.iter(span).next().is_none() {
            for entity in marked_glyphs.iter_many(glyphs.iter()) {
                commands.entity(entity).remove::<Marker>();
            }
        } else {
            for entity in unmarked_glyphs.iter_many(glyphs.iter()) {
                commands.entity(entity).insert(Marker::default());
            }
        }
    }
}

/// This system runs whenever a span's [`Styles`] changes. It checks if the span
/// points to an `Effect`. If it does, then `Marker` is inserted into the span,
/// otherwise it is removed.
pub fn mark_effect_spans<Effect: Component, Marker: Component + Default>(
    mut commands: Commands,
    effect: EffectQuery<&Effect>,
    styles: Query<Entity, Changed<Styles>>,
    marked_spans: Query<Entity, With<Marker>>,
    unmarked_spans: Query<Entity, Without<Marker>>,
) {
    for span in styles.iter() {
        if effect.iter(span).next().is_none()
            && let Ok(entity) = marked_spans.get(span)
        {
            commands.entity(entity).remove::<Marker>();
        } else if let Ok(entity) = unmarked_spans.get(span) {
            commands.entity(entity).insert(Marker::default());
        }
    }
}

/// This system runs whenever a [`Glyph`] appears. It checks if the [`Glyph`]'s span
/// points to an `Effect`. If it does, then `Marker` is inserted into the [`Glyph`],
/// otherwise it is removed.
///
/// [`Glyph`]: pretty_text::glyph::Glyph
pub fn mark_appeared_effect_glyphs<Effect: Component, Marker: Component + Default>(
    trigger: Trigger<OnAdd, Appeared>,
    mut commands: Commands,
    effect: EffectQuery<&Effect>,
    glyphs: Query<(Entity, &GlyphSpan), Without<Marker>>,
) {
    if let Ok((glyph, span)) = glyphs.get(trigger.target())
        && effect.iter(span).next().is_some()
    {
        commands.entity(glyph).insert(Marker::default());
    }
}

#[derive(SystemParam)]
pub struct EffectQuery<'w, 's, D: QueryData + 'static, F: QueryFilter + 'static = ()> {
    query: Query<'w, 's, D, F>,
    span_styles: Query<'w, 's, (Entity, &'static Styles)>,
    style_effects: Query<'w, 's, &'static Effects>,
    registry: Res<'w, StyleRegistry>,
}

impl<'w, 's, D: QueryData + 'static, F: QueryFilter + 'static> EffectQuery<'w, 's, D, F> {
    pub fn iter(
        &self,
        span: impl ContainsEntity,
    ) -> EffectQueryIter<
        impl Iterator<Item = <<D as QueryData>::ReadOnly as QueryData>::Item<'_>>,
        <<D as QueryData>::ReadOnly as QueryData>::Item<'_>,
    > {
        let Ok((span_entity, span_styles)) = self.span_styles.get(span.entity()) else {
            return EffectQueryIter::NoStyles;
        };

        let style_entities = std::iter::once(span_entity).chain(
            span_styles
                .0
                .iter()
                .flat_map(|style| self.registry.get(style.tag.as_ref()))
                .copied(),
        );
        let effect_roots = self.style_effects.iter_many(style_entities);
        EffectQueryIter::Iter(
            self.query
                .iter_many(effect_roots.flat_map(|effects| effects.collection().iter())),
            PhantomData,
        )
    }
}

pub enum EffectQueryIter<I, Item> {
    Iter(I, PhantomData<Item>),
    NoStyles,
}

impl<I, Item> Iterator for EffectQueryIter<I, Item>
where
    I: Iterator<Item = Item>,
{
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Iter(iter, _) => iter.next(),
            Self::NoStyles => None,
        }
    }
}
