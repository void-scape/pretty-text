use crate::effects::material::PrettyTextMaterialAppExt;
use bevy::asset::{load_internal_asset, weak_handle};
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::{AlphaMode2d, Material2d};
use bevy_pretty_text::effects::material::DEFAULT_GLYPH_SHADER_HANDLE;
use pretty_text_macros::{DynamicEffect, GlyphMaterial, parser_syntax};

const RAINBOW_SHADER_HANDLE: Handle<Shader> = weak_handle!("e2bf2b29-bc9e-46d2-a8de-6acb6d0bc534");

pub(super) fn plugin(app: &mut App) {
    load_internal_asset!(
        app,
        RAINBOW_SHADER_HANDLE,
        "rainbow.wgsl",
        Shader::from_wgsl
    );

    app.register_pretty_material::<Rainbow>("rainbow")
        .register_asset_reflect::<Rainbow>()
        .register_type::<Rainbow>();
}

/// Applies scrolling rainbow colors to the opaque pixels in a glyph.
#[derive(Debug, Clone, Asset, AsBindGroup, Reflect, GlyphMaterial, DynamicEffect)]
#[pretty_text(material)]
#[parser_syntax]
pub struct Rainbow {
    /// Font atlas texture handle.
    #[texture(0)]
    #[sampler(1)]
    #[pretty_text(atlas)]
    pub atlas: Handle<Image>,

    /// The speed that colors scroll.
    #[uniform(2)]
    #[syntax(default = 1.0, "{number}")]
    pub speed: f32,

    /// The width of color bands.
    #[uniform(3)]
    #[syntax(default = 1.0, "{number}")]
    pub width: f32,
}

impl Material2d for Rainbow {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(RAINBOW_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}

impl UiMaterial for Rainbow {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(RAINBOW_SHADER_HANDLE)
    }
}
