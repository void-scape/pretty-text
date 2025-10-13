//! This example shows how to combine effects with vertex masking.

use bevy::prelude::*;
use bevy_pretty_text::{glyph::VertexMask, prelude::*};
use prettytext_examples::BalatroPlugin;

fn main() {
    App::default()
        .add_plugins((BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, (camera, spawn_text))
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn spawn_text(mut commands: Commands) {
    commands.spawn((
        PrettyStyle("dance"),
        effects![
            // Only apply the `Wave` effect to the top-left vertex.
            (Wave::default(), VertexMask::TL),
            // Only apply the `Pivot` effect to the bottom-right vertex.
            (Pivot::default(), VertexMask::BR)
        ],
    ));

    commands.spawn((
        pretty2d!("[Boogie Woogie!](dance)"),
        TextFont::from_font_size(52.0),
    ));
}
