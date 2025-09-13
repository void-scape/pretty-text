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
//! [`GlyphMaterial`]: crate::effects::material::GlyphMaterial
//! [`Glyph`]: bevy_pretty_text::glyph::Glyph
//!
//! ### Behavior
//!
//! | Component | Tag | ECS Effect | Material Effect |
//! | --------- | --- | :--------: | :-------------: |
//! | [`Bounce`] | `bounce` | ✅ | ❌ |
//! | [`Breathe`] | `breathe` | ✅ | ❌ |
//! | [`Fade`] | `fade` | ✅ | ❌ |
//! | [`Glitch`] | `glitch` | ❌ | ✅ |
//! | [`Pivot`] | `pivot` | ✅ | ❌ |
//! | [`Rainbow`] | `rainbow` | ❌ | ✅ |
//! | [`Shake`] | `shake` | ✅ | ❌ |
//! | [`Spin`] | `spin` | ✅ | ❌ |
//! | [`Wave`] | `wave` | ✅ | ❌ |
//! | [`Wobble`] | `wobble` | ✅ | ❌ |
//!
//! ### Appearance
//!
//! | Component | Tag | ECS Effect | Material Effect |
//! | --------- | --- | :--------: | :-------------: |
//! | [`Scramble`] | `scramble` | ✅ | ❌ |
//! | [`Spread`] | `spread` | ✅ | ❌ |
//!
//! [`Bounce`]: crate::prelude::Bounce
//! [`Breathe`]: crate::prelude::Breathe
//! [`Fade`]: crate::prelude::Fade
//! [`Glitch`]: crate::prelude::Glitch
//! [`Pivot`]: crate::prelude::Pivot
//! [`Rainbow`]: crate::prelude::Rainbow
//! [`Shake`]: crate::prelude::Shake
//! [`Spin`]: crate::prelude::Spin
//! [`Wave`]: crate::prelude::Wave
//! [`Wobble`]: crate::prelude::Wobble
//!
//! [`Scramble`]: crate::prelude::Scramble
//! [`Spread`]: crate::prelude::Spread

use std::marker::PhantomData;

use bevy::ecs::query::{QueryData, QueryFilter};
use bevy::ecs::system::SystemParam;
use bevy::prelude::*;

use crate::glyph::{SpanGlyphOf, SpanGlyphs};
use crate::prelude::TypewriterSet;
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

        app.init_resource::<dynamic::DynEffectRegistry>()
            .configure_sets(Update, PrettyEffectSet.after(TypewriterSet));
    }
}

/// Tracks which entities are effects of this entity.
///
/// Used by [style entities](crate::style) to track a set of effects.
#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = EffectOf, linked_spawn)]
pub struct Effects(Vec<Entity>);

/// Stores the [style entity](crate::style) this effect entity applies to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[relationship(relationship_target = Effects)]
pub struct EffectOf(pub Entity);

/// Returns a [`SpawnRelatedBundle`] that will insert the [`Effects`] component,
/// spawn a [`SpawnableList`] of entities with given bundles that relate to the
/// [`Effects`] entity via the [`EffectOf`] component, and reserve space in the
/// [`Effects`] for each spawned entity.
///
/// Any additional arguments will be interpreted as bundles to be spawned.
///
/// [`SpawnRelatedBundle`]: bevy::ecs::spawn::SpawnRelatedBundle
/// [`SpawnableList`]: bevy::ecs::spawn::SpawnableList
///
/// ```
/// # use bevy::prelude::*;
/// # use crate::prelude::*;
/// # let mut world = World::new();
/// # let materials = AssetServer::new();
/// commands.spawn((
///     PrettyStyle("my_style"),
///     effects![
///         PrettyTextMaterial(materials.add(Rainbow::default())),
///         Wave::default(),
///     ],
/// ));
/// ```
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
/// [`Glyph`]: crate::glyph::Glyph
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
/// points to an `Effect`. If it does, then `Marker` is inserted into the [`Glyph`].
///
/// [`Glyph`]: crate::glyph::Glyph
pub fn mark_appeared_effect_glyphs<Effect: Component, Marker: Component + Default>(
    trigger: Trigger<OnAdd, Appeared>,
    mut commands: Commands,
    effect: EffectQuery<&Effect>,
    glyphs: Query<(Entity, &SpanGlyphOf), Without<Marker>>,
) {
    if let Ok((glyph, span)) = glyphs.get(trigger.target())
        && effect.get(span).is_ok()
    {
        commands.entity(glyph).insert(Marker::default());
    }
}

/// Error for a [`EffectQuery::get`] operation.
#[derive(Debug, thiserror::Error)]
pub enum EffectQueryEntityError {
    /// Provided entity does not contain the [`Styles`] component.
    #[error("entity does not contain `Styles` component")]
    NoStyles,

    /// [`Styles`] component does not point to any style entities with the target
    /// query data.
    #[error("style entities do not contain the query data: `{0}`")]
    NoEffect(&'static str),
}

/// [`SystemParam`] to query for [glyph effects](mod@crate::effects).
///
/// [`EffectQuery`] performs a [`Query`] over all style entities contained in
/// the [`Styles`] component of a span.
///
/// # Basic Usage
/// ```
/// fn wave(
///     wave_effect_q: EffectQuery<(&Wave, &VertexMask)>,
///     glyph_q: Query<&GlyphSpan, With<Glyph>>,
/// ) {
///     for span_entity in glyphs.iter_mut() {
///         if let Ok(wave_effect) = waves.get(span_entity) {
///             // Apply `wave_effect` to the glyph entity.
///             //
///             // ...
///         }
///     }
/// }
/// ```
#[derive(Debug, SystemParam)]
pub struct EffectQuery<'w, 's, D: QueryData + 'static, F: QueryFilter + 'static = ()> {
    query: Query<'w, 's, D, F>,
    span_styles: Query<'w, 's, (Entity, Option<&'static Styles>, Option<&'static ChildOf>)>,
    style_effects: Query<'w, 's, &'static Effects>,
    registry: Res<'w, StyleRegistry>,
}

impl<'w, 's, D: QueryData + 'static, F: QueryFilter + 'static> EffectQuery<'w, 's, D, F> {
    /// Returns whether or not `span` has the `D` effect query data.
    pub fn is_empty(&self, span: impl ContainsEntity) -> bool {
        self.iter(span).next().is_none()
    }

    /// Fetch the `D` effect query data for `span`.
    ///
    /// Returns an error if no entities match the effect query. If more than one
    /// entity matches the effect query, returns the first match and logs an error.
    pub fn get(
        &self,
        span: impl ContainsEntity,
    ) -> Result<<<D as QueryData>::ReadOnly as QueryData>::Item<'_>, EffectQueryEntityError> {
        let mut iter = self.iter(span);
        match iter.next() {
            Some(item) => {
                if iter.next().is_some() {
                    error!(
                        "text span has multiple of the same effect: {}",
                        std::any::type_name::<D>()
                    );
                }
                Ok(item)
            }
            None => Err(EffectQueryEntityError::NoEffect(std::any::type_name::<D>())),
        }
    }

    /// Iterate over the `D` effect query for `span`.
    pub fn iter(
        &self,
        span: impl ContainsEntity,
    ) -> EffectQueryIter<
        impl Iterator<Item = <<D as QueryData>::ReadOnly as QueryData>::Item<'_>>,
        impl Iterator<Item = <<D as QueryData>::ReadOnly as QueryData>::Item<'_>>,
        <<D as QueryData>::ReadOnly as QueryData>::Item<'_>,
    > {
        let (span_entity, span_styles, child_of) = self.span_styles.get(span.entity()).unwrap();
        if let Some(span_styles) = span_styles {
            let style_entities = std::iter::once(span_entity).chain(
                span_styles
                    .0
                    .iter()
                    .flat_map(|style| self.registry.get(style.tag.as_ref()))
                    .copied(),
            );
            let effect_roots = self.style_effects.iter_many(style_entities);
            EffectQueryIter::Styles(
                self.query.iter_many(
                    [child_of.map(|c| c.parent()), Some(span_entity)]
                        .into_iter()
                        .flatten()
                        .chain(
                            effect_roots.flat_map(|effects| effects.collection().iter().copied()),
                        ),
                ),
                PhantomData,
            )
        } else {
            EffectQueryIter::Root(
                self.query.iter_many(
                    [child_of.map(|c| c.parent()), Some(span_entity)]
                        .into_iter()
                        .flatten(),
                ),
                PhantomData,
            )
        }
    }
}

/// Iterator over a span's style query data.
///
/// Produced by [`EffectQuery::iter`].
#[allow(missing_debug_implementations)]
pub enum EffectQueryIter<S, R, Item> {
    /// Iterator over the text root, span, and style entity's `I` query data.
    Styles(S, PhantomData<Item>),

    /// Iterator over the text root and span entity's `I` query data.
    Root(R, PhantomData<Item>),
}

impl<S, R, Item> Iterator for EffectQueryIter<S, R, Item>
where
    S: Iterator<Item = Item>,
    R: Iterator<Item = Item>,
{
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Styles(styles, _) => styles.next(),
            Self::Root(root, _) => root.next(),
        }
    }
}
