use bevy::asset::load_internal_asset;
use bevy::prelude::*;
use bevy::text::Update2dText;

use material::{DefaultGlyphMaterial, PrettyTextMaterialPlugin};

extern crate self as bevy_pretty_text;

pub mod glyph;
pub mod material;
pub mod parser;
pub mod style;
pub mod type_writer;

#[derive(Debug, SystemSet, PartialEq, Eq, Hash, Clone)]
pub enum PrettyTextSystems {
    GlyphConstruct,
    GlyphPosition,
    Material,
    Style,
}

pub struct PrettyTextCorePlugin;

impl Plugin for PrettyTextCorePlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            material::DEFAULT_GLYPH_SHADER_HANDLE,
            "shaders/default_glyph_material.wgsl",
            Shader::from_wgsl
        );

        app.add_plugins((
            glyph::GlyphMeshPlugin,
            type_writer::TypeWriterPlugin,
            style::StylePlugin,
        ))
        .add_systems(
            PostUpdate,
            material::default_material.in_set(PrettyTextSystems::Material),
        )
        .configure_sets(
            PostUpdate,
            (
                PrettyTextSystems::Style.before(Update2dText),
                PrettyTextSystems::GlyphConstruct.after(Update2dText),
                PrettyTextSystems::GlyphPosition
                    .before(PrettyTextSystems::GlyphConstruct)
                    .after(TransformSystem::TransformPropagate),
                PrettyTextSystems::Material.after(PrettyTextSystems::GlyphPosition),
            ),
        )
        .add_plugins(PrettyTextMaterialPlugin::<DefaultGlyphMaterial>::default());

        app.init_resource::<material::erased::DynMaterialRegistry>()
            //.register_pretty_material::<effects::Wavy>("wavy")
            .add_observer(material::erased::insert_erased_materials);
    }
}

#[derive(Default, Component)]
pub struct PrettyText;
