use bevy::prelude::*;

pub mod scramble;
pub mod shaders;
pub mod shake;

pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            shaders::ShadersPlugin,
            scramble::ScramblePlugin,
            shake::ShakePlugin,
        ));
    }
}
