//! This example demonstrates the simplest way to apply effects to text.
//!
//! See the `typewriter` and `styles` examples for more detail.

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
    // Spawn wavy `Text`.
    commands.spawn((
        Text::new("Hello, World!"),
        Wave::default(),
        TextFont::from_font_size(52.0),
    ));

    // Use the typewriter.
    commands.spawn((
        Typewriter::new(30.),
        Text2d::new("My text is revealed one glyph at a time"),
        Transform::from_xyz(0., 200., 0.),
        TextFont::from_font_size(36.0),
    ));

    // Spawn a style entity.
    commands.spawn((
        PrettyStyle("my_style"),
        TextFont::from_font_size(48.0),
        effects![
            PrettyTextMaterial(materials.add(Rainbow::default())),
            Wave::default(),
        ],
    ));

    // Parse rich text and use custom style.
    commands.spawn((
        pretty!("I am [1]<0.8>*sniff*[1]<1.2> very `pretty`[my_style]![3]<1>"),
        Typewriter::new(10.0),
        TextFont::from_font_size(52.0),
        Node {
            position_type: PositionType::Absolute,
            left: Val::Px(0.0),
            bottom: Val::Px(0.0),
            ..Default::default()
        },
    ));
}
