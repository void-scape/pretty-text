use bevy::asset::{load_internal_asset, weak_handle};
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::{AlphaMode2d, Material2d};
use pretty_text::material::{DEFAULT_GLYPH_SHADER_HANDLE, PrettyTextMaterialAppExt};
use pretty_text_macros::TextMaterial2d;

pub const WAVY_SHADER_HANDLE: Handle<Shader> = weak_handle!("647ec7a9-6325-401a-a0f1-d406223de769");
pub const SHAKY_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("5b5d15ce-feb7-4565-9644-1a0df1c37a40");

pub struct EffectsPlugin;

impl Plugin for EffectsPlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            WAVY_SHADER_HANDLE,
            "shaders/wavy.wgsl",
            Shader::from_wgsl
        );
        load_internal_asset!(
            app,
            SHAKY_SHADER_HANDLE,
            "shaders/shaky.wgsl",
            Shader::from_wgsl
        );

        app.register_pretty_material::<Wavy>("wavy")
            .register_pretty_material::<Shaky>("shaky");
    }
}

#[derive(Default, Clone, Asset, TypePath, AsBindGroup, TextMaterial2d)]
#[pretty_text_path(pretty_text)]
pub struct Wavy {
    #[atlas]
    #[texture(0)]
    #[sampler(1)]
    pub atlas: Handle<Image>,
}

impl Material2d for Wavy {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(WAVY_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}

#[derive(Default, Clone, Asset, TypePath, AsBindGroup, TextMaterial2d)]
#[pretty_text_path(pretty_text)]
pub struct Shaky {
    #[texture(0)]
    #[sampler(1)]
    #[atlas]
    pub atlas: Handle<Image>,

    #[uniform(2)]
    pub intensity: f32,
}

impl Shaky {
    pub fn new(intensity: f32) -> Self {
        Self {
            intensity,
            ..Default::default()
        }
    }
}

impl Material2d for Shaky {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(SHAKY_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}
