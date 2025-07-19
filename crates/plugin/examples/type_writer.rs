//! This example demonstrates how to use the `TypeWriter` component.

mod common;

use bevy::color::palettes::css::RED;
use bevy::prelude::*;
use bevy::text::TextBounds;
use bevy_pretty_text::prelude::*;

fn main() {
    App::new()
        .add_plugins((common::BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, (camera, type_writer))
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn type_writer(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn((PrettyStyle("highlight"), TextColor(RED.into())));

    commands
        .spawn((
            // You can change the reveal mode of the type writer with `TypeWriterMode`.
            //
            // TypeWriterMode::Word,
            TypeWriter::new(15.),
            pretty!(
                "I can `pause`[!highlight][1], <2>`speed up`[!highlight], \
                    [0.5] <0.5>`slow down`[!highlight],[0.5]<1> \
                    {my_event}`emit events`[!highlight],[0.5] and \
                    even {}`run one shot systems`[!highlight]![3]",
                |mut commands: Commands, server: Res<AssetServer>| {
                    commands.spawn(AudioPlayer::new(server.load("bing.wav")));
                }
            ),
            //
            // Style my text with normal Bevy components.
            TextBounds::new_horizontal(750.0),
            TextLayout::new_with_justify(JustifyText::Center),
            TextFont {
                font_size: 52.0,
                font: server.load("Pixelify_Sans/PixelifySans-Regular.ttf"),
                ..Default::default()
            },
        ))
        //
        // `TypeWriterEvent`s are triggered for observers and emitted for `EventReader`s.
        .observe(|trigger: Trigger<TypeWriterEvent>| {
            assert!(trigger.0 == "my_event");
        })
        //
        // This is a convenient place to play audio samples!
        .observe(
            |trigger: Trigger<GlyphRevealed>, mut commands: Commands, server: Res<AssetServer>| {
                if trigger.text != " " {
                    commands.spawn((
                        AudioPlayer::new(server.load("glyph.wav")),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            },
        )
        //
        // When the type writer finishes, it will trigger `TypeWriterFinished` and remove itself
        // from the entity.
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                // Restart by inserting another type writer.
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(15.));
            },
        );
}
