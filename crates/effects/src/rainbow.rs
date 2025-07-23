use bevy::asset::{load_internal_asset, weak_handle};
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::{AlphaMode2d, Material2d};
use bevy_pretty_text::material::DEFAULT_GLYPH_SHADER_HANDLE;
use pretty_text::material::PrettyTextMaterialAppExt;
use pretty_text_macros::GlyphMaterial;

const RAINBOW_SHADER_HANDLE: Handle<Shader> = weak_handle!("e2bf2b29-bc9e-46d2-a8de-6acb6d0bc534");

pub(super) fn plugin(app: &mut App) {
    load_internal_asset!(
        app,
        RAINBOW_SHADER_HANDLE,
        "shaders/rainbow.wgsl",
        Shader::from_wgsl
    );

    app.register_pretty_material::<Rainbow>("rainbow")
        .register_asset_reflect::<Rainbow>()
        .register_type::<Rainbow>();
}

/// Displaces scanlines in a glyph.
///
/// ```
#[doc = include_str!("../docs/header.txt")]
/// # use pretty_text::material::PrettyTextMaterial;
/// #
/// // Parsed usage
/// world.spawn(pretty!("`my text`[rainbow(1)]"));
/// world.spawn(PrettyParser::bundle("`my text`[rainbow(1)]")?);
///
/// // Literal usage
/// world.spawn((
///     Text::new("my text"),
///     PrettyTextMaterial(materials.add(Rainbow {
///         atlas: Default::default(),
///         speed: 1.0,
///     })),
/// ));
#[doc = include_str!("../docs/footer.txt")]
/// ```
#[derive(Debug, Clone, Asset, AsBindGroup, Reflect, GlyphMaterial)]
pub struct Rainbow {
    /// Font atlas texture handle.
    #[texture(0)]
    #[sampler(1)]
    #[pretty_text(atlas)]
    pub atlas: Handle<Image>,

    /// The speed that colors scroll.
    #[uniform(2)]
    pub speed: f32,
}

impl Default for Rainbow {
    fn default() -> Self {
        Self {
            atlas: Default::default(),
            speed: 1f32,
        }
    }
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
