use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
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
        pretty!("`shake`[shake]"),
        font.clone(),
        Transform::from_xyz(-150., 0., 0.),
    ));

    commands.spawn((
        pretty!("`wobble`[wobble(0.3)]"),
        font.clone(),
        Transform::from_xyz(0., -150., 0.),
    ));

    commands
        .spawn((
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
            TypeWriter::new(20.),
            pretty!("`wavy and scrambled`[wavy(5, 20) scrambled][2]"),
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
