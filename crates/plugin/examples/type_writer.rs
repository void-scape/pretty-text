use bevy::prelude::*;
use bevy::text::TextBounds;
use bevy_pretty_text::prelude::*;
use pretty_text::type_writer::TypeWriterEffect;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_systems(
            Startup,
            (
                |mut commands: Commands| {
                    commands.spawn(Camera2d);
                },
                type_writer,
            ),
        )
        .run();
}

fn type_writer(mut commands: Commands) -> Result {
    commands
        .spawn((
            TypeWriter::new(5.),
            Text2d::new("Simple and perfectly wrapped "),
            TextBounds::new_horizontal(100.),
            Transform::from_xyz(0., 150., 0.),
        ))
        .observe(|_: Trigger<GlyphRevealed>| {
            // info!("revealed glyph!");
        })
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                // info!("finished! restarting ...");
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(5.));
            },
        );

    commands
        .spawn((
            TypeWriter::new(2.),
            TypeWriterMode::Word,
            pretty!("Use 'TypeWriterMode' to advance by words or \nglyphs![3]"),
            TextBounds::new_horizontal(200.),
            Transform::from_xyz(0., 0., 0.),
        ))
        .observe(|trigger: Trigger<WordRevealed>| {
            info!("revealed word! `{}`", trigger.text);
        })
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert((TypeWriter::new(2.), TypeWriterMode::Word));
            },
        );

    commands
        .spawn((
            TypeWriter::new(5.),
            pretty!("<0.5>hello \n[1]<1.5>world![1]"),
            Transform::from_xyz(-250., -150., 0.),
        ))
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(5.));
            },
        );

    commands
        .spawn((
            TypeWriter::new(5.),
            Text2d::default(),
            children![
                TypeWriterEffect::Speed(0.5),
                TextSpan::new("hello "),
                TypeWriterEffect::Pause(1.0),
                TypeWriterEffect::Speed(1.5),
                TextSpan::new("world"),
                TextSpan::new("!"),
                TypeWriterEffect::Pause(1.0),
            ],
            Transform::from_xyz(250., -150., 0.),
        ))
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(5.));
            },
        );

    commands
        .spawn((
            TypeWriter::new(4.),
            pretty!("hello {my_event}world![0.5]"),
            Transform::from_xyz(0., -300., 0.),
        ))
        .observe(|trigger: Trigger<TypeWriterEvent>| {
            assert_eq!(trigger.event().0, "my_event");
            // info!("This must be my event!");
        })
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(4.));
            },
        );

    Ok(())
}
