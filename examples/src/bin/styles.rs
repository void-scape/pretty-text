//! This example demonstrates two ways to apply text styles.
//!
//! See the `patch_styles` example for updating styles at run time.

use bevy::color::palettes::css::{GREY, RED};
use bevy::prelude::*;
use bevy_pretty_text::prelude::*;
use prettytext_examples::BalatroPlugin;

fn main() {
    App::default()
        .add_plugins((BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, (spawn_styles, spawn_text))
        .add_observer(apply_my_style)
        .run();
}

fn spawn_styles(mut commands: Commands) {
    // Here, I make a simple text style with a font, color, and wave.
    commands.spawn((
        PrettyStyle("large_red"),
        TextFont {
            font_size: 52.0,
            ..Default::default()
        },
        TextColor(RED.into()),
        effects![Wave::default()],
    ));

    // However, styles are entities, so they can contain *any* component!
    //
    // Note that `MyStyle` needs to implement `Clone` or `Reflect`.
    commands.spawn((PrettyStyle("my_style"), MyStyle));
}

// A component in my `my_style`.
#[derive(Clone, Component)]
struct MyStyle;

fn spawn_text(mut commands: Commands) {
    commands.spawn(Camera2d);

    // Apply my `large_red` style defined above.
    commands.spawn(pretty2d!("`Relationships are easy!`[large_red]"));

    // Apply my `my_style` style defined above.
    commands.spawn((
        pretty2d!("`(#\\[relationship_target\\])`[my_style]"),
        Transform::from_xyz(0.0, -100.0, 0.0),
    ));
}

// An observer to watch for the moment `MyStyle` is cloned from my style entity
// and inserted into a text span.
fn apply_my_style(
    trigger: Trigger<OnAdd, MyStyle>,
    styles: Query<&PrettyStyle>,
    mut commands: Commands,
) {
    // Skip any style entities
    if styles.get(trigger.target()).is_ok() {
        return;
    }

    // I apply `my_style` by making the text grey and bigger
    commands.entity(trigger.target()).insert((
        TextFont {
            font_size: 24.0,
            ..Default::default()
        },
        TextColor(GREY.into()),
    ));
}
