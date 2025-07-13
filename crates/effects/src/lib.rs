#![allow(clippy::type_complexity)]

use bevy::prelude::*;

pub mod scramble;
pub mod shaders;
pub mod shake;
pub mod wave;
pub mod wobble;

extern crate pretty_text as bevy_pretty_text;

pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            shaders::ShadersPlugin,
            scramble::ScramblePlugin,
            shake::ShakePlugin,
            wave::WavePlugin,
            wobble::WobblePlugin,
        ));
    }
}
