//! Provides a powerful API for _styling_ text.
//!
//! [`Styles`] configure the [`TextFont`], [`TextColor`], and [effects] of a [`TextSpan`].
//! [`Style`] is either a [registered style](PrettyStyle) or shorthand for a
//! [dynamic effect] constructor.
//!
//! [effects]: mod@crate::effects
//! [dynamic effect]: crate::effects::dynamic
//!
//! ```no_run
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! # use bevy_pretty_text::style::*;
//! # let mut world = World::new();
//! // Register a `PrettyStyle` in the ECS with a color and effect.
//! world.spawn((
//!     PrettyStyle("my_pretty_style"),
//!     TextColor(Color::BLACK),
//!     effects![Wave {
//!         frequency: 1.2,
//!         height: 2.0,
//!         ..Default::default()
//!     }],
//! ));
//!
//! // Building a `Style` that points to a `my_pretty_style`.
//! let my_pretty_style = Style::from_tag("my_pretty_style");
//!
//! // Building a dynamic effect `Style`.
//! let wave = Style::from_tag("wave")
//!     .with_arg("1.2")
//!     .with_named_arg("height", "2");
//!
//! // Applying `my_pretty_style` and `wave` to a text span.
//! world.spawn((
//!     Text2d::default(),
//!     children![(
//!         TextSpan::new("Hello, world!"),
//!         Styles::new([my_pretty_style, wave])
//!     )],
//! ));
//! ```
//!
//! The [pretty text parser](crate::parser) is a simple abstraction over building
//! these styled text hierarchies.
//!
//! ```no_run
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! # let mut world = World::new();
//! // Register a `PrettyStyle` in the ECS with a color and effect.
//! world.spawn((
//!     PrettyStyle("my_pretty_style"),
//!     TextColor(Color::BLACK),
//!     effects![Wave {
//!         frequency: 1.2,
//!         height: 2.0,
//!         ..Default::default()
//!     }],
//! ));
//!
//! // Applying `my_pretty_style` to a text span.
//! world.spawn(
//!     pretty!("[Hello, world!](my_pretty_style)"),
//! );
//! ```
//!
//! [See here for more information.](crate::parser#ecs-structure)
//!
//! ## Default Styles
//!
//! | Name    | [`TextColor`]                   |
//! | ------- | ------------------------------- |
//! | `blue` | `TextColor(Color::from(BLUE))` |
//! | `fuchsia` | `TextColor(Color::from(FUCHSIA))` |
//! | `gray` | `TextColor(Color::from(GRAY))` |
//! | `green` | `TextColor(Color::from(GREEN))` |
//! | `lime` | `TextColor(Color::from(LIME))` |
//! | `maroon` | `TextColor(Color::from(MAROON))` |
//! | `navy` | `TextColor(Color::from(NAVY))` |
//! | `olive` | `TextColor(Color::from(OLIVE))` |
//! | `purple` | `TextColor(Color::from(PURPLE))` |
//! | `red` | `TextColor(Color::from(RED))` |
//! | `silver` | `TextColor(Color::from(SILVER))` |
//! | `teal` | `TextColor(Color::from(TEAL))` |
//! | `white` | `TextColor(Color::from(WHITE))` |
//! | `yellow` | `TextColor(Color::from(YELLOW))` |

use std::borrow::Cow;
use std::fmt::{Debug, Write};

use bevy::ecs::component::HookContext;
use bevy::ecs::entity::EntityClonerBuilder;
use bevy::ecs::system::{SystemChangeTick, SystemParam};
use bevy::ecs::world::DeferredWorld;
use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::text::Update2dText;
use bevy::ui::UiSystem;

use crate::effects::dynamic::{DynEffectRegistry, TrackedSpan};
use crate::effects::{EffectOf, Effects};
use crate::parser::Root;

/// Systems for style change detection and styling spans.
///
/// Runs in the [`PostUpdate`] schedule.
#[derive(Debug, SystemSet, PartialEq, Eq, Hash, Clone)]
pub struct PrettyStyleSet;

/// Enables styling text with the [`PrettyStyle`] components.
///
/// [See here for a list of the default styles.](crate::style#default-styles)
#[derive(Debug)]
pub struct StylePlugin;

impl Plugin for StylePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<StyleRegistry>()
            .add_systems(PreStartup, spawn_default_styles)
            .add_systems(
                PostUpdate,
                (detect_style_entity_changes, apply_styles)
                    .chain()
                    .in_set(PrettyStyleSet),
            )
            .register_type::<PrettyStyle>()
            .register_type::<Styles>();

        app.configure_sets(
            PostUpdate,
            PrettyStyleSet
                // systems that check if the tree needs to be recomputed
                .before(Update2dText)
                .before(UiSystem::Content),
        );
    }
}

/// Registers a [style entity](crate::style).
///
/// ```no_run
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     PrettyStyle("my_style"),
///     TextColor(Color::WHITE),
///     TextFont {
///         font_size: 32.0,
///         ..Default::default()
///     },
/// ));
/// ```
///
/// # Defining Styles
///
/// [`Styles`](crate::parser#styles) *are* entities. The components in a style entity are cloned
/// into text spans.
///
/// Despawning a style entity unregisters the style.
///
/// All text spans will first inherit their parent's font and color before
/// applying any styles.
///
/// Don't forgot to derive `Clone` and or [`Reflect`] for style components,
/// otherwise they will not appear in your text spans.
///
/// ```no_run
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # let mut world = World::new();
/// // Here I am defining `my_style` with a color.
/// world.spawn((
///     PrettyStyle("my_style"),
///     TextColor(Color::WHITE),
/// ));
///
/// // Here the `TextColor` from `my_style` will be cloned
/// // into this span!
/// world.spawn((
///     pretty!("[My text span](my_style)"),
/// //                          ^^^^^^^^
/// ));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Component, Reflect)]
#[component(immutable, on_add = register, on_remove = unregister, on_replace = replace)]
pub struct PrettyStyle(pub &'static str);

impl AsRef<str> for PrettyStyle {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl From<&'static str> for PrettyStyle {
    fn from(value: &'static str) -> Self {
        Self(value)
    }
}

/// A comma separated collection of [styles](Style) contained within square brackets:
/// `"[style1, style2, ...]"`, directly following a [text span](crate::parser::Span).
///
/// [`Styles`] configure the [`TextFont`], [`TextColor`], and [effects] of a [`TextSpan`].
/// Changes to [`Styles`] are automatically tracked and applied. Adding or removing a
/// [`Style`] will force the [text layout](bevy::text::TextLayoutInfo) to recompute.
///
/// [effects]: mod@crate::effects
#[derive(Debug, Default, Clone, Component, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Styles(pub Vec<Style>);

impl Styles {
    /// Create a new [`Styles`] from `styles`.
    pub fn new(styles: impl IntoIterator<Item = Style>) -> Self {
        Self(styles.into_iter().collect())
    }

    /// Create a new [`Styles`] from `style`.
    pub fn from_style(style: Style) -> Self {
        Self(vec![style])
    }
}

/// A dynamic representation of a [`PrettyStyle`].
///
/// To apply a [`Style`] to a [`TextSpan`], insert it into the [`Styles`] component
/// or use the [`StyleUiWriter`] or [`Style2dWriter`] system params.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Style {
    /// The `tag` can represent either a [`PrettyStyle`] entity or a [dynamic effect].
    ///
    /// [dynamic effect]: crate::effects::dynamic::PrettyTextEffectAppExt
    pub tag: Tag,

    /// Field arguments for a dynamic effect.
    pub args: Vec<Arg>,
}

impl Style {
    /// Create a new [`Style`] from `tag`.
    #[inline]
    pub fn from_tag(tag: impl Into<Tag>) -> Self {
        Self {
            tag: tag.into(),
            ..Default::default()
        }
    }

    /// Append an [`Arg::Positioned`] with `value`.
    #[inline]
    pub fn with_arg(mut self, value: impl Into<Arg>) -> Self {
        self.args.push(value.into());
        self
    }

    /// Append an [`Arg::Named`] with `value`.
    #[inline]
    pub fn with_named_arg(mut self, name: impl AsRef<str>, value: impl AsRef<str>) -> Self {
        self.args.push(Arg::Named {
            field: Cow::Owned(name.as_ref().to_string()),
            value: Cow::Owned(value.as_ref().to_string()),
        });
        self
    }
}

impl std::fmt::Display for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.args.is_empty() {
            write!(f, "{}", self.tag)
        } else {
            write!(f, "{}", self.tag)?;
            f.write_char('(')?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    f.write_char(',')?;
                }
                write!(f, "{arg}")?;
            }
            f.write_char(')')
        }
    }
}

impl From<&'static str> for Style {
    fn from(value: &'static str) -> Self {
        Self::from_tag(value)
    }
}

impl From<String> for Style {
    fn from(value: String) -> Self {
        Self::from_tag(value)
    }
}

impl From<Cow<'static, str>> for Style {
    fn from(value: Cow<'static, str>) -> Self {
        Self::from_tag(value)
    }
}

/// Tag associated to a [registered dynamic effect].
///
/// [registered dynamic effect]: crate::effects::dynamic::PrettyTextEffectAppExt
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Tag(Cow<'static, str>);

impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_ref())
    }
}

impl AsRef<str> for Tag {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<&'static str> for Tag {
    fn from(value: &'static str) -> Self {
        Self(Cow::Borrowed(value))
    }
}

impl From<String> for Tag {
    fn from(value: String) -> Self {
        Self(Cow::Owned(value))
    }
}

impl From<Cow<'static, str>> for Tag {
    fn from(value: Cow<'static, str>) -> Self {
        Self(value)
    }
}

/// Field arguments for a dynamic effect.
///
/// See [`ArgParser`](crate::parser::ArgParser) for a description of the argument syntax.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub enum Arg {
    /// Positional argument: `value`.
    Positioned(Cow<'static, str>),
    /// Named argument: `field=value`.
    Named {
        /// Name of a field.
        field: Cow<'static, str>,
        /// Value to assign to `field`.
        value: Cow<'static, str>,
    },
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Positioned(arg) => f.write_str(arg.as_ref()),
            Self::Named { field, value } => {
                write!(f, "{}={}", field.as_ref(), value.as_ref())
            }
        }
    }
}

impl From<&'static str> for Arg {
    fn from(value: &'static str) -> Self {
        Self::Positioned(value.into())
    }
}

impl From<String> for Arg {
    fn from(value: String) -> Self {
        Self::Positioned(value.into())
    }
}

/// [`StyleWriter`] for [`Text2d`] entities.
pub type Style2dWriter<'w, 's> = StyleWriter<'w, 's, Text2d>;
/// [`StyleWriter`] for [`Text`] entities.
pub type StyleUiWriter<'w, 's> = StyleWriter<'w, 's, Text>;

/// [`SystemParam`] to update text styles.
///
/// # Basic Usage
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::style::StyleUiWriter;
/// fn style_writer(target: Single<Entity, With<Text>>, mut writer: StyleUiWriter) -> Result {
///     // Iterate over all text spans of the text entity and replace the `style_to_replace`
///     // with `replacement_style`.
///     writer
///         // This will fail if the provided entity does not have a `Text` component.
///         .iter_spans_mut(*target)?
///         .replace("style_to_replace", "replacement_style");
///
///     Ok(())
/// }
/// ```
#[derive(Debug, SystemParam)]
pub struct StyleWriter<'w, 's, R: Root> {
    roots: Query<'w, 's, &'static Children, With<R>>,
    span_styles: Query<'w, 's, &'static mut Styles>,
}

impl<'w, 's, R: Root> StyleWriter<'w, 's, R> {
    /// Iterate over the text spans of the `root` text entity.
    ///
    /// Fails if the `root` text entity does not contain `R` or has no [`Children`].
    pub fn iter_spans_mut(
        &mut self,
        root: impl ContainsEntity,
    ) -> Result<SpanStyleWriter<'_, 'w, 's>> {
        Ok(SpanStyleWriter {
            children: self.roots.get(root.entity())?,
            span_styles: &mut self.span_styles,
        })
    }
}

/// Utility for modifying the styles of a text hierarchy.
///
/// Created by [`StyleWriter::iter_spans_mut`].
#[derive(Debug)]
pub struct SpanStyleWriter<'a, 'w, 's> {
    children: &'a Children,
    span_styles: &'a mut Query<'w, 's, &'static mut Styles>,
}

impl<'a, 'w, 's> SpanStyleWriter<'a, 'w, 's> {
    /// Remove all styles from the text hierarchy.
    pub fn clear(&mut self) -> &mut Self {
        self.for_each(|mut styles| {
            if !styles.0.is_empty() {
                styles.0.clear();
            }
        })
    }

    /// Push `style` into all text spans.
    pub fn push(&mut self, style: impl Into<Style>) -> &mut Self {
        let style = style.into();
        self.for_each(|mut styles| {
            styles.0.push(style.clone());
        })
    }

    /// Replace `target` style with `new` style in all text spans.
    pub fn replace(&mut self, target: &'static str, new: impl Into<Style>) -> &mut Self {
        let new = new.into();
        self.for_each(|mut styles| {
            let mut is_changed = false;
            for style in styles.bypass_change_detection().0.iter_mut() {
                if style.tag.as_ref() == target {
                    *style = new.clone();
                    is_changed = true;
                }
            }
            if is_changed {
                styles.set_changed();
            }
        })
    }

    fn for_each(&mut self, mut f: impl FnMut(Mut<Styles>)) -> &mut Self {
        let mut styles = self.span_styles.iter_many_mut(self.children.iter());
        while let Some(style) = styles.fetch_next() {
            f(style);
        }
        self
    }
}

// Caches style entities. The registry is synced with styles in the ECS.
#[derive(Debug, Default, Deref, DerefMut, Resource)]
pub(crate) struct StyleRegistry(pub HashMap<&'static str, Entity>);

fn register(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    let mut registry = world.resource_mut::<StyleRegistry>();

    if registry.0.contains_key(tag) {
        error!("Style `{}` is already registered", tag);
    }

    registry.insert(tag, ctx.entity);
}

fn unregister(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    world.resource_mut::<StyleRegistry>().remove(tag);
}

fn replace(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    let mut registry = world.resource_mut::<StyleRegistry>();
    registry.remove(tag);

    if registry.0.contains_key(tag) {
        error!("Style `{}` is already registered", tag);
    }

    registry.insert(tag, ctx.entity);
}

fn spawn_default_styles(mut commands: Commands) {
    fn bundle(root: Entity, name: &'static str, style: &'static str, color: Srgba) -> impl Bundle {
        (
            ChildOf(root),
            Name::new(name),
            PrettyStyle(style),
            TextColor(Color::from(color)),
        )
    }

    let root = commands.spawn(Name::new("Default Pretty Styles")).id();
    use bevy::color::palettes::basic::*;
    for bundle in [
        bundle(root, "Aqua", "aqua", AQUA),
        bundle(root, "Black", "black", BLACK),
        bundle(root, "Blue", "blue", BLUE),
        bundle(root, "Fuchsia", "fuchsia", FUCHSIA),
        bundle(root, "Gray", "gray", GRAY),
        bundle(root, "Green", "green", GREEN),
        bundle(root, "Lime", "lime", LIME),
        bundle(root, "Maroon", "maroon", MAROON),
        bundle(root, "Navy", "navy", NAVY),
        bundle(root, "Olive", "olive", OLIVE),
        bundle(root, "Purple", "purple", PURPLE),
        bundle(root, "Red", "red", RED),
        bundle(root, "Silver", "silver", SILVER),
        bundle(root, "Teal", "teal", TEAL),
        bundle(root, "White", "white", WHITE),
        bundle(root, "Yellow", "yellow", YELLOW),
    ]
    .into_iter()
    {
        commands.spawn(bundle);
    }
}

fn apply_styles(
    mut commands: Commands,
    registry: Res<AppTypeRegistry>,
    server: Res<AssetServer>,
    effect_registry: Res<DynEffectRegistry>,
    style_registry: Res<StyleRegistry>,
    styles: Query<(Entity, &Styles, Option<&ChildOf>, Option<&TrackedSpan>), Changed<Styles>>,
) -> Result {
    for (span, styles, child_of, tracked) in styles.iter() {
        commands.entity(span).despawn_related::<Effects>();

        // inherit first
        if let Some(child_of) = child_of {
            commands.entity(child_of.parent()).clone_with(
                span,
                |config: &mut EntityClonerBuilder| {
                    config.deny_all().allow::<(TextFont, TextColor)>();
                },
            );
        }

        for style in styles.0.iter() {
            if let Some(handler) = effect_registry.get(style.tag.as_ref()) {
                let effect_entity = commands.spawn((EffectOf(span), ChildOf(span))).id();
                if let Err(mut err) = handler.insert_from_args(
                    &registry,
                    &server,
                    &mut commands.entity(effect_entity),
                    &style.args,
                ) {
                    if let Some(tracked) = tracked {
                        err.tracked(tracked.location());
                    }
                    return Err(err.into());
                }
            } else if let Some(entity) = style_registry.get(style.tag.as_ref()) {
                if !style.args.is_empty() {
                    error!(
                        "Expected no arguments for style `{}`. \
                        If this is supposed to be an effect, it is not registered",
                        style.tag.as_ref()
                    );
                }

                commands
                    .entity(*entity)
                    .clone_with(span, |config: &mut EntityClonerBuilder| {
                        config.deny::<(PrettyStyle, Effects, EffectOf, Children, ChildOf)>();
                    });
            } else {
                error!("Style `{}` is not registered", style.tag.as_ref());
            }
        }
    }

    Ok(())
}

fn detect_style_entity_changes(
    mut spans: Query<Mut<Styles>>,
    style_entities: Query<EntityRef, (With<PrettyStyle>, Without<Styles>)>,
    registry: Res<StyleRegistry>,
    system_ticks: SystemChangeTick,
) {
    for entity in style_entities.iter() {
        if entity.archetype().components().any(|id| {
            entity.get_change_ticks_by_id(id).is_some_and(|ticks| {
                ticks.is_changed(system_ticks.last_run(), system_ticks.this_run())
            })
        }) {
            for mut styles in spans.iter_mut() {
                if styles.0.iter().any(|style| {
                    registry
                        .get(style.tag.as_ref())
                        .is_some_and(|e| *e == entity.id())
                }) {
                    styles.set_changed();
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use bevy::ecs::system::RunSystemOnce;
    use bevy::prelude::*;

    use super::StylePlugin;
    use crate::effects::dynamic::DynEffectRegistry;
    use crate::prelude::*;
    use crate::style::{Style, StyleRegistry, Styles};

    fn app() -> App {
        let mut app = App::new();
        app.add_plugins((MinimalPlugins, AssetPlugin::default(), StylePlugin))
            .init_resource::<DynEffectRegistry>();
        app.finish();
        app
    }

    #[test]
    fn style_registry() {
        let mut app = app();

        let style_entity = app
            .world_mut()
            .spawn((PrettyStyle("style"), TextColor(Color::WHITE)))
            .id();

        app.update();

        let registry = app.world().resource::<StyleRegistry>();
        assert!(registry.contains_key("style"));
        assert_eq!(*registry.get("style").unwrap(), style_entity);

        app.world_mut().entity_mut(style_entity).despawn();
        app.update();

        let registry = app.world().resource::<StyleRegistry>();
        assert!(registry.get("style").is_none());
    }

    #[test]
    fn style_change_detection() {
        let mut app = app();

        let styles_entity = app
            .world_mut()
            .spawn(Styles::from_style(Style::from_tag("style")))
            .id();
        let style_entity = app
            .world_mut()
            .spawn((PrettyStyle("style"), Sprite::default()))
            .id();

        app.update();
        app.update();

        let styles = app
            .world()
            .entity(styles_entity)
            .get_ref::<Styles>()
            .unwrap();
        assert!(!styles.is_changed());
        let mut entity = app.world_mut().entity_mut(style_entity);
        let mut sprite = entity.get_mut::<Sprite>().unwrap();
        sprite.color = Color::BLACK;

        app.world_mut()
            .run_system_once(super::detect_style_entity_changes)
            .unwrap();

        let styles = app
            .world()
            .entity(styles_entity)
            .get_ref::<Styles>()
            .unwrap();
        assert!(styles.is_changed());
    }
}
