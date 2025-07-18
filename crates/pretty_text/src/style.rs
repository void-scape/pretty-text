//! Provides custom text styling with [`PrettyStyle`] entities.
//!
//! Initializes the following styles:
//! | Name    | [`TextColor`]      |
//! | ------- | ------------------ |
//! | `blue`  | `TextColor`(blue)  |
//! | `green` | `TextColor`(green) |
//! | `red`   | `TextColor`(red)   |

use std::borrow::Cow;

use bevy::ecs::component::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::parser::{Modifier, Modifiers};

/// Enables styling text with the [`PrettyStyle`] and [`SpanStyle`] components.
///
/// See [`style`](crate::style) for the default styles.
#[derive(Debug)]
pub struct StylePlugin;

impl Plugin for StylePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<PrettyStyleRegistry>()
            .add_systems(PreStartup, default_styles)
            .add_observer(apply_span_style);

        app.register_type::<PrettyStyleRegistry>()
            .register_type::<SpanStyle>();
    }
}

fn default_styles(mut commands: Commands) {
    use bevy::color::palettes::css::{BLUE, GREEN, RED};
    commands.spawn_batch([
        (PrettyStyle("blue"), TextColor(Color::from(BLUE))),
        (PrettyStyle("green"), TextColor(Color::from(GREEN))),
        (PrettyStyle("red"), TextColor(Color::from(RED))),
    ]);
}

/// Marks a [style entity](crate::style).
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::style::*;
/// #
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     PrettyStyle,
///     Name::new("my_style"),
///     TextColor(Color::WHITE),
///     TextFont {
///         font_size: 32.0,
///         ..Default::default(),
///     }
/// ));
/// ```
///
/// # Parser Syntax
///
/// **Spans** are ranges of text, denoted with backticks: ``"`...`"``.
///
/// **Modifiers** are a comma seperated collection of effects and styles, which
/// directly follow a **span** and are contained in square brackets: `"[mod1, ...]"`.
///
/// **Styles** are a modifier, prefixed with `!`.
///
/// See [`parser`](bevy_pretty_text::parser).
///
/// ## Examples
///
/// ``"`I am a styled span`[!my_style]"``
///
/// ``"`I am a doubly styled span`[!first_style, !second_style]"``
///
/// # Defining Styles
///
/// **Styles** *are* entities. The components in a style entity are cloned
/// into text spans.
///
/// All text spans will first inherit their parent's font and color before
/// applying any styles.
///
/// Don't forgot to derive `Clone` and or [`Reflect`] for style components,
/// otherwise they will not appear in your text spans.
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::style::*;
/// # use pretty_text::pretty;
/// #
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
///     pretty!("`My text span`[!my_style]"),
///                              ^^^^^^^^
/// ));
/// ```
#[derive(Debug, Component)]
#[component(on_add = register, on_remove = unregister)]
pub struct PrettyStyle(pub &'static str);

/// Indicates how a span should be styled.
///
/// Used by [`bevy_pretty_text::parser`] to dynamically style spans.
#[derive(Debug, Clone, PartialEq, Eq, Component, Reflect)]
pub enum SpanStyle {
    /// An entity with `SpanStyle::Style` will query for the associated
    /// [`PrettyStyle`] entity.
    ///
    /// If the style is found, its components are cloned into this entity.
    Style(Cow<'static, str>),

    /// An entity with `SpanStyle::StyleSet` will query for the associated
    /// [`PrettyStyle`] entities, applying each style in order.
    ///
    /// Components from styles later in the collection will override previous values.
    StyleSet(Vec<Cow<'static, str>>),
}

impl SpanStyle {
    /// The number of styles.
    pub fn styles(&self) -> usize {
        match self {
            Self::Style(_) => 1,
            Self::StyleSet(set) => set.len(),
        }
    }

    /// Efficiently flatten a collection of styles.
    pub fn flatten(styles: Vec<Self>) -> Self {
        let len: usize = styles.iter().map(|style| style.styles()).sum();
        if len == 1 {
            debug_assert_eq!(styles.len(), 1);
            return match styles.into_iter().next().unwrap() {
                Self::Style(style) => Self::Style(style),
                Self::StyleSet(set) => Self::Style(set.into_iter().next().unwrap()),
            };
        }

        let mut collection = Vec::with_capacity(len);
        for style in styles.into_iter() {
            style.insert(&mut collection);
        }

        if collection.len() == 1 {
            Self::Style(collection.into_iter().next().unwrap())
        } else {
            Self::StyleSet(collection)
        }
    }

    fn insert(self, collection: &mut Vec<Cow<'static, str>>) {
        match self {
            Self::Style(style) => collection.push(style),
            Self::StyleSet(set) => collection.extend(set),
        }
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for SpanStyle {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Style(tag) => {
                quote::quote! {
                    bevy_pretty_text::style::SpanStyle::Style(std::borrow::Cow::Borrowed(#tag))
                }
            }
            Self::StyleSet(set) => {
                quote::quote! {
                    bevy_pretty_text::style::SpanStyle::StyleSet(
                        vec![#(std::borrow::Cow::Borrowed(#set),)*]
                    )
                }
            }
        });
    }
}

#[derive(Debug, Default, Resource, Reflect)]
struct PrettyStyleRegistry(HashMap<&'static str, Entity>);

fn register(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    world
        .resource_mut::<PrettyStyleRegistry>()
        .0
        .insert(tag, ctx.entity);
}

fn unregister(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    world.resource_mut::<PrettyStyleRegistry>().0.remove(tag);
}

fn apply_span_style(
    trigger: Trigger<OnAdd, Modifiers>,
    mut commands: Commands,
    mods: Query<(Entity, &Modifiers, &ChildOf), Added<Modifiers>>,
    text_components: Query<(&TextFont, &TextColor)>,
    registry: Res<PrettyStyleRegistry>,
) {
    let Ok((entity, mods, child_of)) = mods.get(trigger.target()) else {
        return;
    };

    let styles = mods
        .0
        .iter()
        .filter_map(|m| match m {
            Modifier::Style(style) => Some(style.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    // inherit font and color first
    if let Ok((font, color)) = text_components.get(child_of.0) {
        commands.entity(entity).insert((font.clone(), *color));
    }

    match SpanStyle::flatten(styles) {
        SpanStyle::Style(style) => {
            if let Some(style_entity) = registry.0.get(style.as_ref()) {
                commands.entity(*style_entity).clone_with(entity, |config| {
                    config.deny::<PrettyStyle>();
                });
            } else {
                error!("style `{}` not found", style.as_ref());
            }
        }
        SpanStyle::StyleSet(styles) => {
            for style in styles.into_iter() {
                if let Some(style_entity) = registry.0.get(style.as_ref()) {
                    commands.entity(*style_entity).clone_with(entity, |config| {
                        config.deny::<PrettyStyle>();
                    });
                } else {
                    error!("style `{}` not found", style.as_ref());
                }
            }
        }
    }
}
