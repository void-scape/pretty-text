use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, bevy_pretty_text::PrettyTextPlugin))
        .add_systems(Startup, mesh)
        .run();
}

fn mesh(mut commands: Commands) {
    commands.spawn(Camera2d);

    commands
        .spawn((
            TypeWriter::cps(5.),
            Text2d::new("Hello!"),
            Transform::from_xyz(0., -200., 0.),
        ))
        .observe(|_: Trigger<GlyphRevealed>| {
            info!("revealed glyph!");
        })
        .observe(|_: Trigger<TypeWriterFinished>| {
            info!("finished!");
        });
}
