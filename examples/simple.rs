use bevy::color::palettes::css::GREEN;
use bevy::prelude::*;
use bevy::text::TextBounds;
use bevy_pretty_text::prelude::*;
use pretty_text_effects::shaders::{Wobble, Wavy};

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, bevy_pretty_text::PrettyTextPlugin))
        .add_systems(
            Startup,
            (
                |mut commands: Commands| {
                    commands.spawn(Camera2d);
                },
                simple,
            ),
        )
        .run();
}

fn simple(
    mut commands: Commands,
    mut shaky: ResMut<Assets<Wobble>>,
    mut wavy: ResMut<Assets<Wavy>>,
) {
    commands.spawn((
        PrettyText,
        Text2d::new("Just some normal text"),
        TextBounds::new_horizontal(100.),
        TextColor(GREEN.into()),
        Transform::from_xyz(-150., -75., 0.),
    ));

    commands.spawn((
        PrettyTextMaterial(shaky.add(Wobble::new(0.01))),
        Text2d::new("Cool and Wobble!"),
        Transform::from_xyz(150., -75., 0.),
    ));

    commands.spawn((
        PrettyText,
        Text2d::new("Composed of "),
        children![
            TextSpan::new("several "),
            (
                TextSpan::new("spans!"),
                PrettyTextMaterial(wavy.add(Wavy::default())),
                TextColor(GREEN.into())
            )
        ],
        Transform::from_xyz(0., 75., 0.),
    ));
}
