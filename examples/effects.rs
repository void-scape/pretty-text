use bevy::prelude::*;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pretty_text::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_plugins((
            bevy_egui::EguiPlugin {
                enable_multipass_for_primary_context: true,
            },
            WorldInspectorPlugin::new(),
        ))
        .add_systems(
            Startup,
            (
                |mut commands: Commands| {
                    commands.spawn(Camera2d);
                },
                effects,
            ),
        )
        .run();
}

fn effects(mut commands: Commands) {
    let font = TextFont {
        font_size: 36.,
        ..Default::default()
    };

    commands.spawn((
        Name::new("Shake"),
        pretty!("`shake`[shake]"),
        font.clone(),
        Transform::from_xyz(-150., 0., 0.),
    ));

    commands.spawn((
        Name::new("Glitch"),
        pretty!("`glitch`[glitch]"),
        font.clone(),
        Transform::from_xyz(-250., -250., 0.),
    ));

    commands.spawn((
        Name::new("Wobble"),
        pretty!("`wobble`[wobble(0.7, 1.5)]"),
        font.clone(),
        Transform::from_xyz(0., -150., 0.),
    ));

    commands
        .spawn((
            Name::new("Scrambled"),
            TypeWriter::new(20.),
            font.clone(),
            pretty!("`scrambled`[scrambled][2]"),
            Transform::from_xyz(150., 0., 0.),
        ))
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(20.));
            },
        );

    commands
        .spawn((
            Name::new("Wave and Scrambled"),
            TypeWriter::new(20.),
            pretty!("`wave and scrambled`[wave(1, 20) scrambled][2]"),
            font.clone(),
            Transform::from_xyz(0., 150., 0.),
        ))
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(20.));
            },
        );
}
