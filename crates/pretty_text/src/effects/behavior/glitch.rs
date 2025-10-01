use bevy::asset::{load_internal_asset, uuid_handle};
use bevy::prelude::*;
use bevy::render::render_resource::AsBindGroup;
use bevy::shader::ShaderRef;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::effects::material::{GlyphMaterial, PrettyTextMaterialAppExt};

const GLITCH_SHADER_HANDLE: Handle<Shader> = uuid_handle!("5b5d15ce-feb7-4565-9644-1a0df1c37a40");

pub(super) fn plugin(app: &mut App) {
    load_internal_asset!(app, GLITCH_SHADER_HANDLE, "glitch.wgsl", Shader::from_wgsl);

    app.register_pretty_material::<Glitch>("glitch")
        .register_asset_reflect::<Glitch>();
}

/// Displaces scanlines in a glyph.
#[derive(Debug, Clone, Asset, AsBindGroup, Reflect, DynamicEffect)]
#[pretty_text(material)]
#[parser_syntax]
pub struct Glitch {
    /// Maximum displacement.
    #[uniform(0)]
    #[syntax(default = 0.02, "{number}")]
    pub intensity: f32,

    /// Number of potential scanlines.
    #[uniform(1)]
    #[syntax(default = 150.0, "{number}")]
    pub frequency: f32,

    /// How fast the glitch changes.
    #[uniform(2)]
    #[syntax(default = 8.0, "{number}")]
    pub speed: f32,

    /// Minimum threshold for glitch to occur.
    #[uniform(3)]
    #[syntax(default = 0.85, "{number}")]
    pub threshold: f32,
}

impl GlyphMaterial for Glitch {
    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(GLITCH_SHADER_HANDLE)
    }
}
