//use bevy::asset::{load_internal_asset, weak_handle};
use bevy::prelude::*;
//use bevy::render::render_resource::{AsBindGroup, ShaderRef};
//use bevy::sprite::{AlphaMode2d, Material2d};
//use pretty_text::material::{DEFAULT_GLYPH_SHADER_HANDLE, PrettyTextMaterialAppExt};
//use pretty_text_macros::TextMaterial2d;

//pub const WOBBLE_SHADER_HANDLE: Handle<Shader> =
//    weak_handle!("5b5d15ce-feb7-4565-9644-1a0df1c37a40");

pub struct ShadersPlugin;

impl Plugin for ShadersPlugin {
    fn build(&self, _app: &mut App) {
        //load_internal_asset!(
        //    app,
        //    WOBBLE_SHADER_HANDLE,
        //    "shaders/wobble.wgsl",
        //    Shader::from_wgsl
        //);
        //app.register_pretty_material::<Wobble>("wobble");
    }
}

//#[derive(Default, Clone, Asset, TypePath, AsBindGroup, TextMaterial2d)]
//#[pretty_text_path(pretty_text)]
//pub struct Wobble {
//    #[texture(0)]
//    #[sampler(1)]
//    #[atlas]
//    pub atlas: Handle<Image>,
//
//    #[uniform(2)]
//    pub intensity: f32,
//}
//
//impl Wobble {
//    pub fn new(intensity: f32) -> Self {
//        Self {
//            intensity,
//            ..Default::default()
//        }
//    }
//}
//
//impl Material2d for Wobble {
//    fn vertex_shader() -> ShaderRef {
//        ShaderRef::Handle(WOBBLE_SHADER_HANDLE)
//    }
//
//    fn fragment_shader() -> ShaderRef {
//        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
//    }
//
//    fn alpha_mode(&self) -> AlphaMode2d {
//        AlphaMode2d::Blend
//    }
//}
