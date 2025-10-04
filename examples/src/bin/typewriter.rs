//! This example demonstrates how to use the typewriter.
//!
//! Press space or enter to pause and unpause the typewriter.

use bevy::color::palettes::css::RED;
use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::new()
        .add_plugins((prettytext_examples::BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, (camera, typewriter))
        .add_systems(Update, pause)
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

// System to pause and unpause execution of the `Typewriter`.
fn pause(
    mut commands: Commands,
    input: Res<ButtonInput<KeyCode>>,
    typewriter: Option<Single<Entity, With<Typewriter>>>,
    mut paused: Local<bool>,
) {
    let Some(typewriter) = typewriter else {
        return;
    };

    if input.just_pressed(KeyCode::Space) || input.just_pressed(KeyCode::Enter) {
        if *paused {
            // Removing `PauseTypewriter` resumes execution.
            commands.entity(*typewriter).remove::<PauseTypewriter>();
        } else {
            // Inserting `PauseTypewriter` pauses execution indefinitely.
            commands.entity(*typewriter).insert(PauseTypewriter);
        }
        *paused = !*paused;
    }
}

fn typewriter(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn((PrettyStyle("highlight"), TextColor(RED.into())));

    let typewriter = commands
        .spawn((
            // Reveeal word-by-word instead of glyph-by-glyph.
            //
            // TypewriterIndex::word(),
            Typewriter::new(20.),
            pretty!(
                "I can [pause](red),|1| <2>[speed up](shake, green),\
                    |0.5| <0.5>[slow down](wobble, yellow),|0.5|<1> \
                    {my_event}[emit events](bounce, fuchsia),|0.5| and \
                    even {}[run one shot systems](rainbow)!|3|",
                |mut commands: Commands, server: Res<AssetServer>| {
                    commands.spawn(AudioPlayer::new(server.load("bing.wav")));
                },
            ),
            Spread::default(),
            //
            // Style my text with normal Bevy components.
            TextLayout::new_with_justify(Justify::Center),
            TextFont {
                font_size: 52.0,
                font: server.load("Pixelify_Sans/PixelifySans-Regular.ttf"),
                ..Default::default()
            },
            //
            // Position with Bevy's Node ui component
            Node {
                width: Val::Percent(50f32),
                height: Val::Percent(50f32),
                align_self: AlignSelf::Center,
                ..Default::default()
            },
        ))
        //
        // `TypewriterEvent`s are triggered for observers and emitted for `MessageReader`s.
        .observe(|event: On<Revealed<TypewriterEvent>>| {
            assert!(event.event().0 == "my_event");
        })
        //
        // This is a convenient place to play audio samples!
        .observe(
            |revealed: On<Revealed<Char>>, mut commands: Commands, server: Res<AssetServer>| {
                if revealed.event().text != " " {
                    commands.spawn((
                        AudioPlayer::new(server.load("glyph.wav")),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            },
        )
        //
        // When the type writer finishes, it will trigger `TypewriterFinished` and remove itself
        // from the entity.
        .observe(|finished: On<TypewriterFinished>, mut commands: Commands| {
            // Restart by inserting another type writer.
            commands
                .entity(finished.event().entity)
                .insert(Typewriter::new(15.));
        })
        .id();

    commands
        .spawn(Node {
            width: Val::Percent(100f32),
            height: Val::Percent(100f32),
            justify_content: JustifyContent::Center,
            ..Default::default()
        })
        .add_child(typewriter);
}
