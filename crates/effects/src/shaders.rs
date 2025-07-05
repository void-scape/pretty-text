use bevy::asset::{load_internal_asset, weak_handle};
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::{AlphaMode2d, Material2d};
use pretty_text::material::PrettyTextMaterialAppExt;
use pretty_text_macros::TextMaterial2d;

pub const HOLOGRAPHIC_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("5b5d15ce-feb7-4565-9644-1a0df1c37a40");

pub struct ShadersPlugin;

impl Plugin for ShadersPlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            HOLOGRAPHIC_SHADER_HANDLE,
            "shaders/glitch.wgsl",
            Shader::from_wgsl
        );

        app.register_pretty_material::<Glitch>("glitch");

        app.register_asset_reflect::<Glitch>()
            .register_type::<Glitch>();
    }
}

#[derive(Clone, Asset, AsBindGroup, TextMaterial2d, Reflect)]
#[pretty_text_path(pretty_text)]
pub struct Glitch {
    #[texture(0)]
    #[sampler(1)]
    #[material(atlas)]
    pub atlas: Handle<Image>,

    /// Maximum displacement.
    #[uniform(2)]
    pub intensity: f32,

    /// Number of potential scanlines.
    #[uniform(3)]
    pub frequency: f32,

    /// How fast the glitch changes.
    #[uniform(4)]
    pub speed: f32,

    /// Minimum threshold for glitch to occur.
    #[uniform(5)]
    pub threshold: f32,
}

impl Default for Glitch {
    fn default() -> Self {
        Self {
            atlas: Default::default(),
            intensity: 0.02,
            frequency: 150.0,
            speed: 8.0,
            threshold: 0.85,
        }
    }
}

impl Material2d for Glitch {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(HOLOGRAPHIC_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(HOLOGRAPHIC_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}
