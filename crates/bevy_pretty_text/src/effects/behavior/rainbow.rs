use bevy::asset::{load_internal_asset, uuid_handle};
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderType};
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
#[uniform(0, RainbowUniform)]
pub struct Rainbow {
    /// The speed that colors scroll.
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// The width of color bands.
    #[syntax(default = 1.0, "{number}")]
    pub width: f32,
}

impl GlyphMaterial for Rainbow {
    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(RAINBOW_SHADER_HANDLE)
    }
}

#[derive(ShaderType)]
struct RainbowUniform {
    speed: f32,
    width: f32,
    _pad0: u32,
    _pad1: u32,
}

impl From<&Rainbow> for RainbowUniform {
    fn from(value: &Rainbow) -> Self {
        Self {
            speed: value.speed,
            width: value.width,
            _pad0: 0,
            _pad1: 0,
        }
    }
}
