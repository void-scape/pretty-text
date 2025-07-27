use bevy::asset::{load_internal_asset, weak_handle};
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::{AlphaMode2d, Material2d};
use bevy_pretty_text::material::DEFAULT_GLYPH_SHADER_HANDLE;
use pretty_text::material::PrettyTextMaterialAppExt;
use pretty_text_macros::{DynamicEffect, GlyphMaterial, dynamic_effect_docs};

const GLITCH_SHADER_HANDLE: Handle<Shader> = weak_handle!("5b5d15ce-feb7-4565-9644-1a0df1c37a40");

pub(super) fn plugin(app: &mut App) {
    load_internal_asset!(
        app,
        GLITCH_SHADER_HANDLE,
        "shaders/glitch.wgsl",
        Shader::from_wgsl
    );

    app.register_pretty_material::<Glitch>("glitch")
        .register_asset_reflect::<Glitch>()
        .register_type::<Glitch>();
}

/// Displaces scanlines in a glyph.
#[derive(Debug, Clone, Asset, AsBindGroup, Reflect, GlyphMaterial, DynamicEffect)]
#[pretty_text(material)]
#[dynamic_effect_docs]
pub struct Glitch {
    /// Font atlas texture handle.
    #[texture(0)]
    #[sampler(1)]
    #[pretty_text(atlas)]
    pub atlas: Handle<Image>,

    /// Maximum displacement.
    #[uniform(2)]
    #[syntax(default = 0.02, "{number}")]
    pub intensity: f32,

    /// Number of potential scanlines.
    #[uniform(3)]
    #[syntax(default = 150.0, "{number}")]
    pub frequency: f32,

    /// How fast the glitch changes.
    #[uniform(4)]
    #[syntax(default = 8.0, "{number}")]
    pub speed: f32,

    /// Minimum threshold for glitch to occur.
    #[uniform(5)]
    #[syntax(default = 0.85, "{number}")]
    pub threshold: f32,
}

impl Material2d for Glitch {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(GLITCH_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}

impl UiMaterial for Glitch {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(GLITCH_SHADER_HANDLE)
    }
}
