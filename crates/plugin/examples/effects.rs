//! This example demonstrates how to use text effects.

mod common;

use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::default()
        .add_plugins((common::BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, spawn_text)
        .run();
}

fn spawn_text(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn(Camera2d);

    commands.spawn((
        pretty!("`To `Single`[!red] or Not To `Single`[!red]`[wobble]"),
        //                                                    ^^^^^^
        // Unlike *styles*, effects are not prefixed by `!`.
        TextFont {
            font_size: 38.0,
            ..Default::default()
        },
        Transform::from_xyz(0.0, 200.0, 0.0),
    ));

    // Layer and interleave multiple effects
    commands.spawn((
        pretty!(
            "ヽ(\\`Д)ノ \
            ``EVERYTHING`[!red, scramble(12, always), shake] AS ENTITIES`[wave] \
            ヽ(\\`Д)ノ",
            // ^^^ back ticks and other delimiters can be escaped with "//"
        ),
        TextFont {
            font_size: 38.0,
            font: server.load("Noto_Sans_JP/NotoSansJP-Regular.ttf"),
            ..Default::default()
        },
    ));

    // Supply any number of arguments and the rest are defaulted.
    commands.spawn((
        pretty!("`I cant believe its not `bsn`[!red]`[shake(1.5, 2.5)]"),
        //                                            ^^^^^^^^^^^^^^^
        // This is syntax sugar for struct literals, and equivalent to:
        //
        // Shake {
        //     intensity: 2.0,
        //     radius: 2.5,
        //     ..Default::default()
        // },
        //
        TextFont {
            font_size: 38.0,
            ..Default::default()
        },
        Transform::from_xyz(0.0, -200.0, 0.0),
    ));
}
