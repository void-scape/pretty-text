use bevy::prelude::*;
use bevy::window::WindowResolution;

mod audio;
mod sequence;
mod textbox;

fn main() {
    App::default()
        .add_plugins((
            DefaultPlugins
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        resolution: WindowResolution::new(768.0 * 1.5, 560.0 * 1.5),
                        ..Default::default()
                    }),
                    ..Default::default()
                })
                .set(ImagePlugin::default_nearest()),
            bevy_pretty_box::PrettyBoxPlugin,
            bevy_seedling::SeedlingPlugin::default(),
            // bevy_inspector_egui::bevy_egui::EguiPlugin::default(),
            // bevy_inspector_egui::quick::WorldInspectorPlugin::default(),
        ))
        .add_plugins((
            audio::AudioPlugin,
            textbox::TextboxPlugin,
            sequence::SequencePlugin,
        ))
        .run();
}
