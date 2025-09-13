//! This example demonstrates the simplest way to apply effects to text.
//!
//! See the `effects` and `styles` examples for more detail.

use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::default()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_systems(Startup, (camera, spawn_text))
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn spawn_text(mut commands: Commands, mut materials: ResMut<Assets<Rainbow>>) {
    commands.spawn((
        Text2d::new("Wavy Text2d"),
        Fade::default(),
        TextFont::from_font_size(52.0),
    ));

    commands.spawn((
        Text::new("Rainbow Text"),
        PrettyTextMaterial(materials.add(Rainbow::default())),
        TextFont::from_font_size(52.0),
    ));
}
