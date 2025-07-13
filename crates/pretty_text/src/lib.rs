use bevy::asset::load_internal_asset;
use bevy::prelude::*;
use bevy::text::Update2dText;

use material::{DefaultGlyphMaterial, PrettyTextMaterialPlugin};

extern crate self as bevy_pretty_text;

pub mod access;
pub mod bundle;
pub mod dynamic_effects;
pub mod glyph;
pub mod material;
pub mod parser;
pub mod style;
pub mod type_writer;

#[derive(Debug, SystemSet, PartialEq, Eq, Hash, Clone)]
pub enum PrettyTextSystems {
    GlyphConstruct,
    Material,
    Style,

    GlyphPosition,
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
            .init_resource::<dynamic_effects::DynEffectRegistry>()
            .add_observer(dynamic_effects::text_effect)
            .add_observer(material::erased::insert_erased_materials);

        app.register_type::<PrettyText>()
            .register_type::<material::Material>()
            .register_type::<material::DefaultGlyphMaterial>();
    }
}

#[derive(Debug, Default, Component, Reflect)]
pub struct PrettyText;
