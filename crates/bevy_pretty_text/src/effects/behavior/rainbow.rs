use bevy::asset::{load_internal_asset, uuid_handle};
use bevy::prelude::*;
use bevy::render::render_resource::AsBindGroup;
use bevy::shader::ShaderRef;
use pretty_text_macros::{DynamicEffect, parser_syntax};

use crate::effects::material::{GlyphMaterial, PrettyTextMaterialAppExt};

const RAINBOW_SHADER_HANDLE: Handle<Shader> = uuid_handle!("e2bf2b29-bc9e-46d2-a8de-6acb6d0bc534");

pub(super) fn plugin(app: &mut App) {
    load_internal_asset!(
        app,
        RAINBOW_SHADER_HANDLE,
        "rainbow.wgsl",
        Shader::from_wgsl
    );

    app.register_pretty_material::<Rainbow>("rainbow")
        .register_asset_reflect::<Rainbow>();
}

/// Applies scrolling rainbow colors to the opaque pixels in a glyph.
#[derive(Debug, Clone, Asset, AsBindGroup, Reflect, DynamicEffect)]
#[pretty_text(material)]
#[parser_syntax]
pub struct Rainbow {
    /// The speed that colors scroll.
    #[uniform(0)]
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// The width of color bands.
    #[uniform(1)]
    #[syntax(default = 1.0, "{number}")]
    pub width: f32,
}

impl GlyphMaterial for Rainbow {
    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(RAINBOW_SHADER_HANDLE)
    }
}
