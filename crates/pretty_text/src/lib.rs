//! The core crate for the `pretty_text` ecosystem.
//!
//! See [`bevy_pretty_text`](https://docs.rs/bevy_pretty_text) for a high level
//! overview of the available crates and features.

#![allow(clippy::too_many_arguments, clippy::type_complexity)]
#![warn(missing_debug_implementations, missing_docs, clippy::doc_markdown)]

use bevy::prelude::*;

extern crate self as bevy_pretty_text;

pub mod access;
pub mod dynamic_effects;
pub mod glyph;
pub mod material;
pub mod parser;
pub mod style;
pub mod type_writer;

/// Top level text component.
///
/// `PrettyText` enables text from a [`Text2d`] hiearchy to be converted into
/// [`Glyph`](glyph::Glyph)s.
///
/// Special [ECS](dynamic_effects) and [shader](material) driven effects can
/// then be applied to these `Glyph`s. `Glyph`s can also be [styled](style) with
/// normal `Bevy` text components ([`TextFont`], [`TextColor`]) and additionally
/// configured with [style entities](style::PrettyStyle).
///
/// All of this behavior can be encoded with a special syntax and [parsed](parser)
/// at run-time or compile-time.
///
/// All components that rely on `Glyph`s should require `PrettyText`.
#[derive(Debug, Default, Component, Reflect)]
pub struct PrettyText;

/// Inserts the necessary infastructure to process the [glyph] and
/// [type writer](type_writer) logic.
#[derive(Debug)]
pub struct PrettyTextCorePlugin;

impl Plugin for PrettyTextCorePlugin {
    fn build(&self, app: &mut App) {
        #[cfg(not(test))]
        {
            use bevy::asset::load_internal_asset;
            load_internal_asset!(
                app,
                material::DEFAULT_GLYPH_SHADER_HANDLE,
                "shaders/default_glyph_material.wgsl",
                Shader::from_wgsl
            );
        }

        app.add_plugins((
            glyph::GlyphMeshPlugin,
            type_writer::TypeWriterPlugin,
            style::StylePlugin,
        ))
        .init_resource::<dynamic_effects::DynEffectRegistry>()
        .add_observer(dynamic_effects::text_effect)
        .register_type::<PrettyText>();

        material::plugin(app);
    }
}

// #[cfg(test)]
// mod test {
//     use bevy::{ecs::system::RunSystemOnce, prelude::*};
//
//     pub fn prepare_app<F: IntoSystem<(), (), M>, M>(startup: F) -> App {
//         let mut app = App::new();
//
//         app.add_plugins((
//             MinimalPlugins,
//             AssetPlugin::default(),
//             super::PrettyTextCorePlugin,
//         ))
//         .add_systems(Startup, startup);
//
//         app.finish();
//         app.cleanup();
//
//         app
//     }
//
//     pub fn run<F: IntoSystem<(), O, M>, O, M>(app: &mut App, system: F) -> O {
//         let world = app.world_mut();
//         world.run_system_once(system).unwrap()
//     }
// }
