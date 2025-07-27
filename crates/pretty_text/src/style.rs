//! Provides custom text styling with [`PrettyStyle`] entities.
//!
//! Initializes several built-in styles:
//! | Name    | [`TextColor`]                   |
//! | ------- | ------------------------------- |
//! | `blue`  | `TextColor(Color::from(BLUE))`  |
//! | `green` | `TextColor(Color::from(GREEN))` |
//! | `red`   | `TextColor(Color::from(RED))`   |

use bevy::ecs::component::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::platform::collections::HashMap;
use bevy::prelude::*;

/// Enables styling text with the [`PrettyStyle`] components.
///
/// See [`style`](crate::style) for the default styles.
#[derive(Debug)]
pub struct StylePlugin;

impl Plugin for StylePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<PrettyStyleRegistry>()
            .add_systems(PreStartup, default_styles)
            .register_type::<PrettyStyle>();
    }
}

fn default_styles(mut commands: Commands) {
    use bevy::color::palettes::css::{BLUE, GREEN, RED};
    commands.spawn_batch([
        (
            Name::new("Blue Pretty Style"),
            PrettyStyle("blue"),
            TextColor(Color::from(BLUE)),
        ),
        (
            Name::new("Green Pretty Style"),
            PrettyStyle("green"),
            TextColor(Color::from(GREEN)),
        ),
        (
            Name::new("Red Pretty Style"),
            PrettyStyle("red"),
            TextColor(Color::from(RED)),
        ),
    ]);
}

/// Marks a [style entity](crate::style).
///
/// ```no_run
/// # use bevy::prelude::*;
/// # use pretty_text::style::*;
/// #
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
/// # use pretty_text::style::*;
#[doc = include_str!("../docs/pretty.txt")]
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
///     pretty!("`My text span`[my_style]"),
/// //                          ^^^^^^^^
/// ));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Component, Reflect)]
#[component(on_add = register, on_remove = unregister)]
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

/// Caches style entities.
///
/// The registry is synced with styles in the ECS.
#[derive(Debug, Default, Deref, DerefMut, Resource)]
pub(crate) struct PrettyStyleRegistry(pub HashMap<&'static str, Entity>);

fn register(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    let mut registry = world.resource_mut::<PrettyStyleRegistry>();

    if registry.0.contains_key(tag) {
        error!("style `{}` is already registered", tag);
    }

    registry.insert(tag, ctx.entity);
}

fn unregister(mut world: DeferredWorld, ctx: HookContext) {
    let tag = world.get::<PrettyStyle>(ctx.entity).unwrap().0;
    world.resource_mut::<PrettyStyleRegistry>().remove(tag);
}

#[cfg(test)]
mod test {
    use bevy::prelude::*;

    use crate::modifier::{Modifier, Modifiers};
    use crate::test::{prepare_app, run, run_tests};

    use super::PrettyStyle;

    #[derive(Component, Clone)]
    struct MyStyle;

    #[derive(Component)]
    struct NonCloneStyle;

    #[test]
    fn insert_style() {
        run_tests(
            || {
                let mut app = prepare_app();
                app.world_mut()
                    .spawn((PrettyStyle("style"), MyStyle, NonCloneStyle));
                app
            },
            |app, entity, _| {
                app.world_mut()
                    .entity_mut(entity)
                    .insert(Modifiers(vec![Modifier {
                        tag: "style".into(),
                        args: Vec::new(),
                    }]));

                app.world_mut().run_schedule(PostUpdate);
                app.world_mut().flush();
                run(
                    app,
                    move |style: Query<&MyStyle>, non_clone: Query<&NonCloneStyle>| {
                        assert_eq!(
                            style.iter().len(),
                            2,
                            "expected 2, got {}",
                            style.iter().len()
                        );
                        assert!(
                            non_clone.single().is_ok(),
                            "expected 1, got {}",
                            non_clone.iter().len()
                        );
                    },
                );
            },
        )
    }
}
