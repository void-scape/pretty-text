use std::marker::PhantomData;

use bevy::asset::weak_handle;
use bevy::prelude::*;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::{AlphaMode2d, Material2d, Material2dPlugin};

use crate::glyph::{GlyphOf, GlyphSpanEntity, SpanAtlasImage};
use crate::{PrettyText, PrettyTextSystems};

pub const DEFAULT_GLYPH_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("35d4f25c-eb2b-4f26-872f-ef666a76554e");

#[derive(Default, Clone, Component)]
#[require(PrettyText, ContainsPrettyTextMaterial, InvalidMaterialHandle)]
pub struct PrettyTextMaterial<M: TextMaterial2d>(pub Handle<M>);

pub trait TextMaterial2d: Material2d {
    fn set_atlas(&mut self, atlas: Handle<Image>);
}

pub trait PrettyTextMaterialAppExt {
    fn register_text_material<T>(&mut self, tag: &'static str) -> &mut Self
    where
        T: TextMaterial2d + erased::DynTextMaterial2d,
        T::Data: PartialEq + Eq + std::hash::Hash + Clone;
}

impl PrettyTextMaterialAppExt for App {
    fn register_text_material<T>(&mut self, tag: &'static str) -> &mut Self
    where
        T: TextMaterial2d + erased::DynTextMaterial2d,
        T::Data: PartialEq + Eq + std::hash::Hash + Clone,
    {
        self.add_plugins(PrettyTextMaterialPlugin::<T>::default())
            .add_systems(PreStartup, erased::register_dyn_material::<T>(tag))
    }
}

pub struct PrettyTextMaterialPlugin<T>(PhantomData<T>);

impl<T> Default for PrettyTextMaterialPlugin<T> {
    fn default() -> Self {
        PrettyTextMaterialPlugin(PhantomData)
    }
}

impl<T> Plugin for PrettyTextMaterialPlugin<T>
where
    T: TextMaterial2d,
    T::Data: PartialEq + Eq + std::hash::Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.add_plugins(Material2dPlugin::<T>::default())
            .add_systems(
                PostUpdate,
                (
                    apply_material::<T>,
                    set_material_atlas::<T>,
                    // Workaround for bevyengine/bevy#19048, ensuring the mesh components are
                    // present before extraction-relevant systems.
                    ApplyDeferred,
                )
                    .before(bevy::sprite::check_entities_needing_specialization::<T>)
                    .after(default_material)
                    .in_set(PrettyTextSystems::Material),
            );
    }
}

#[derive(Default, Clone, Asset, TypePath, AsBindGroup)]
pub struct DefaultGlyphMaterial {
    #[texture(0)]
    #[sampler(1)]
    pub atlas: Handle<Image>,
}

impl Material2d for DefaultGlyphMaterial {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}

impl TextMaterial2d for DefaultGlyphMaterial {
    fn set_atlas(&mut self, atlas: Handle<Image>) {
        self.atlas = atlas;
    }
}

#[derive(Default, Component)]
pub(super) struct ContainsPrettyTextMaterial;

pub(super) fn default_material(
    mut commands: Commands,
    unmaterialized_text: Query<Entity, (With<PrettyText>, Without<ContainsPrettyTextMaterial>)>,
    mut materials: ResMut<Assets<DefaultGlyphMaterial>>,
) {
    for entity in unmaterialized_text.iter() {
        commands.entity(entity).insert(PrettyTextMaterial(
            materials.add(DefaultGlyphMaterial::default()),
        ));
    }
}

fn apply_material<T: TextMaterial2d>(
    mut commands: Commands,
    glyphs: Query<
        (Entity, &GlyphSpanEntity),
        (
            With<GlyphOf>,
            Without<ContainsPrettyTextMaterial>,
            Without<PrettyTextMaterial<T>>,
        ),
    >,
    spans: Query<&PrettyTextMaterial<T>>,
) {
    for (entity, span_entity) in glyphs.iter() {
        if let Ok(material) = spans.get(span_entity.0) {
            commands
                .entity(entity)
                .insert(MeshMaterial2d(material.0.clone()));
        }
    }
}

#[derive(Default, Component)]
struct InvalidMaterialHandle;

fn set_material_atlas<T: TextMaterial2d>(
    mut commands: Commands,
    text: Query<(Entity, &PrettyTextMaterial<T>, &SpanAtlasImage), With<InvalidMaterialHandle>>,
    mut materials: ResMut<Assets<T>>,
) {
    for (entity, material, atlas) in text.iter() {
        if let Some(material) = materials.get_mut(&material.0) {
            material.set_atlas(atlas.0.clone());
            commands.entity(entity).remove::<InvalidMaterialHandle>();
        }
    }
}

pub mod erased {
    use bevy::platform::collections::HashMap;

    use super::*;

    pub type BoxedDynMaterial =
        Box<dyn Fn(&[String], &mut EntityCommands, &AssetServer) -> Result + Send + Sync + 'static>;

    pub trait DynTextMaterial2d {
        fn dyn_text_material() -> BoxedDynMaterial;
    }

    #[derive(Default, Deref, DerefMut, Resource)]
    pub(crate) struct DynMaterialRegistry(pub HashMap<&'static str, BoxedDynMaterial>);

    pub(crate) fn register_dyn_material<T: DynTextMaterial2d>(
        tag: &'static str,
    ) -> impl Fn(ResMut<DynMaterialRegistry>) {
        move |mut registry| {
            registry.insert(tag, T::dyn_text_material());
        }
    }

    #[derive(Default, Clone, Component)]
    #[require(PrettyText, ContainsPrettyTextMaterial, InvalidMaterialHandle)]
    pub struct ErasedPrettyTextMaterial {
        pub tag: String,
        pub args: Vec<String>,
    }

    #[cfg(feature = "proc-macro")]
    impl quote::ToTokens for ErasedPrettyTextMaterial {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            use quote::TokenStreamExt;
            let tag = &self.tag;
            let args = &self.args;
            tokens.append_all(quote::quote! {
                ::bevy_pretty_text::material::erased::ErasedPrettyTextMaterial::Tag {
                    tag: #tag.into(),
                    args: vec![#(#args.into(),)*]
                }
            });
        }
    }

    pub(crate) fn insert_erased_materials(
        trigger: Trigger<OnAdd, ErasedPrettyTextMaterial>,
        mut commands: Commands,
        server: Res<AssetServer>,
        materials: Query<&ErasedPrettyTextMaterial>,
        registry: Res<DynMaterialRegistry>,
    ) -> Result {
        let material = materials.get(trigger.target())?;
        let handler = registry.get(material.tag.as_str()).ok_or_else(|| {
            format!(
                "failed to insert text material: `{}` is not registered",
                material.tag
            )
        })?;

        handler(
            &material.args,
            &mut commands.entity(trigger.target()),
            &server,
        )?;

        Ok(())
    }
}
