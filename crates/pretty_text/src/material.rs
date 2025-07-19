//! Material [`effects`] are `Bevy` [assets](bevy::asset) that are dynamically
//! constructed at run time and inserted into text hierarchies.
//!
//! Material effects refer to shader driven effects, such as `glitch`. For ECS
//! effects, see [`dynamic_effects`](crate::dynamic_effects).
//!
//! # Using Material Effects
//!
//! ```
//! # use bevy::prelude::*;
#![doc = include_str!("docs/pretty")]
//! #
//! # let mut world = World::new();
//! // Built-in effects are provided with the `default_effects` feature!
//! world.spawn(pretty!("`my glitchy text span`[glitch]"));
//! // ...
//!
//! // Default arguments example
//!
//! #[derive(Default)]
//! struct Glitch {
//!     atlas: Handle<Image>,
//!     arg1: usize,
//!     arg2: f32,
//! }
//!
//! world.spawn(
//!     pretty!("`my shaky text span`[glitch(10)]"),
//! //                     arg2 is defaulted! -^
//! );
//!
//! // This is just syntax sugar for:
//! Glitch {
//!     atlas: Handle::default(),
//! //  ^^^^^ The atlas field is always skipped!
//!     arg1: 10,
//!     ..Default::default()
//! };
//! ```
//!
//! # Defining Custom Materials
//!
//! Material effects are normal `Bevy` [`Material2d`] types that contain a special
//! `atlas` field. The `atlas` field is a handle to the glyph atlas asset which
//! allows glyphs to directly sample from the glyph atlas in the fragment shader.
//!
//! The glyph meshes are packed with extra vertex data to define the position and
//! size of a glyph in the glyph atlas texture.
//!
//! Refer to `Bevy`'s [`Material2d`] or the [default glyph shader] to get started
//! with writing your custom shader.
//!
//! [default glyph shader]: https://github.com/void-scape/pretty-text/blob/a0a0a5631b9302d1db292b9e19d6955809835633/crates/pretty_text/src/shaders/default_glyph_material.wgsl
//!
//! ```ignore
//! #[derive(Clone, Asset, TypePath, AsBindGroup, TextMaterial2d)]
//! pub struct MyMaterial {
//!     /// Font atlas texture handle.
//!     #[texture(0)]
//!     #[sampler(1)]
//!     #[text_material(atlas)] // <-- You must provide an `atlas` field
//!     pub atlas: Handle<Image>,
//!
//!     /// My uniform data.
//!     #[uniform(2)]
//!     pub intensity: f32,
//! }
//!
//! // Provide default values for the derived `TextMaterial2d`.
//! impl Default for MyMaterial {
//!     fn default() -> Self {
//!         Self {
//!             atlas: Default::default(),
//!             intensity: 0.02,
//!         }
//!     }
//! }
//!
//! // Implement `Bevy`'s `Material2d` trait.
//! impl Material2d for MyMaterial {
//!     fn vertex_shader() -> ShaderRef {
//!         // Write a custom vertex shader
//!         "my_shader.wgsl".into()
//!         // Or use the default glyph vertex shader
//!         // DEFAULT_GLYPH_SHADER_HANDLE.into()
//!     }
//!
//!     fn fragment_shader() -> ShaderRef {
//!         // Write a custom fragment shader
//!         "my_shader.wgsl".into()
//!         // Or use the default glyph fragment shader
//!         // DEFAULT_GLYPH_SHADER_HANDLE.into()
//!     }
//!
//!     fn alpha_mode(&self) -> AlphaMode2d {
//!         AlphaMode2d::Blend
//!     }
//! }
//!
//! // Registering `MyEffect`.
//! app.register_pretty_material::<MyMaterial>("my_effect");
//!
//! // Using `MyMaterial`.
//! world.spawn(pretty!("`my text span`[my_effect]"));
//! ```

use std::borrow::Cow;
use std::marker::PhantomData;

use bevy::asset::weak_handle;
use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::sprite::{Material2d, Material2dPlugin};

use crate::PrettyText;
use crate::glyph::{GlyphSystems, SpanAtlasImage};

pub(super) fn plugin(app: &mut App) {
    sealed::plugin(app);

    app.init_resource::<DynMaterialRegistry>()
        .register_type::<ErasedPrettyTextMaterial>();
}

/// The default shader for [`Glyph`](crate::glyph::Glyph)s.
///
/// Custom text materials can use the default vertex or fragment shader if no
/// special behavior is required.
pub const DEFAULT_GLYPH_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("35d4f25c-eb2b-4f26-872f-ef666a76554e");

/// A special [material](Material2d) used for rendering a [`Glyph`](crate::glyph::Glyph).
///
/// See [`Material2d`] for general information about 2D materials.
///
/// See [`material`](crate::material) for general information about text materials
/// and how to implement your own.
#[derive(Debug, Default, Clone, Component, Reflect)]
#[require(PrettyText, sealed::Material::new::<Self>())]
pub struct PrettyTextMaterial<M: TextMaterial2d>(pub Handle<M>);

/// A special [`Material2d`] that renders [`Glyph`](crate::glyph::Glyph)s.
///
/// This trait should derived with [`TextMaterial2d`](pretty_text_macros::TextMaterial2d),
/// which provides a [`DynamicTextMaterial`] implementation.
///
/// See [`Material2d`] for general information about 2D materials.
///
/// See [`material`](crate::material) for general information about text materials
/// and how to implement your own.
pub trait TextMaterial2d: Material2d {
    /// Assigns this material's atlas.
    fn set_atlas(&mut self, atlas: Handle<Image>);
}

/// Extension trait for registering [text materials](crate::material).
pub trait PrettyTextMaterialAppExt {
    /// Register material `T` with a `tag`.
    fn register_pretty_material<T>(&mut self, tag: &'static str) -> &mut Self
    where
        T: Default + TextMaterial2d + DynamicTextMaterial,
        T::Data: PartialEq + Eq + std::hash::Hash + Clone;
}

impl PrettyTextMaterialAppExt for App {
    fn register_pretty_material<T>(&mut self, tag: &'static str) -> &mut Self
    where
        T: Default + TextMaterial2d + DynamicTextMaterial,
        T::Data: PartialEq + Eq + std::hash::Hash + Clone,
    {
        self.register_type::<PrettyTextMaterial<T>>();

        self.add_plugins(PrettyTextMaterialPlugin::<T>::default())
            .add_systems(PreStartup, sealed::register_dyn_material::<T>(tag))
    }
}

/// Adds the necessary ECS resources and render logic to enable rendering entities
/// using the given [`TextMaterial2d`] asset type.
pub struct PrettyTextMaterialPlugin<T>(PhantomData<T>);

impl<T> Default for PrettyTextMaterialPlugin<T> {
    fn default() -> Self {
        PrettyTextMaterialPlugin(PhantomData)
    }
}

impl<T> std::fmt::Debug for PrettyTextMaterialPlugin<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PrettyTextMaterialPlugin")
            .field(&std::any::type_name::<T>())
            .finish()
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
                    sealed::apply_material::<T>,
                    set_material_atlas::<T>,
                    // Workaround for bevyengine/bevy#19048, ensuring the mesh components are
                    // present before extraction-relevant systems.
                    ApplyDeferred,
                )
                    .before(bevy::sprite::check_entities_needing_specialization::<T>)
                    .after(sealed::default_material)
                    .in_set(GlyphSystems::PropagateMaterial),
            );
    }
}

/// Constructs `Self` from `args` and inserts into an entity.
///
/// See [`material`](crate::material).
///
/// This trait should be derived with [`TextMaterial2d`](pretty_text_macros::TextMaterial2d).
pub trait DynamicTextMaterial: Send + Sync + 'static {
    /// Construct a dynamic material from `args` and insert into `entity`.
    ///
    /// Returns a [`BevyError`] if the material can not constructed from `args`.
    fn insert_from_args(
        &self,
        args: &[Cow<'static, str>],
        entity: &mut EntityCommands,
        server: &AssetServer,
    ) -> Result<()>;
}

/// A dynamic representation of a text material.
///
/// Used by [`bevy_pretty_text::parser`] to dynamically insert text materials.
#[derive(Debug, Default, Clone, Component, Reflect)]
pub struct ErasedPrettyTextMaterial {
    /// Tag associated to a [registered material](PrettyTextMaterialAppExt).
    pub tag: Cow<'static, str>,

    /// Field arguments for a dynamic material.
    pub args: Vec<Cow<'static, str>>,
}

/// Dynamic material registry.
///
/// See [`material`](crate::material).
#[derive(Default, Deref, DerefMut, Resource)]
pub struct DynMaterialRegistry(pub HashMap<&'static str, Box<dyn DynamicTextMaterial>>);

impl std::fmt::Debug for DynMaterialRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DynMaterialRegistry")
            .field(&self.0.keys())
            .finish()
    }
}

/// Propagate the glyph atlas handle from span entities to glyph entities.
pub fn set_material_atlas<T: TextMaterial2d>(
    text: Query<
        (&PrettyTextMaterial<T>, &SpanAtlasImage),
        Or<(
            Added<SpanAtlasImage>,
            Added<PrettyTextMaterial<T>>,
            Changed<SpanAtlasImage>,
            Changed<PrettyTextMaterial<T>>,
        )>,
    >,
    mut materials: ResMut<Assets<T>>,
) {
    for (material, atlas) in text.iter() {
        if let Some(material) = materials.get_mut(&material.0) {
            material.set_atlas(atlas.0.clone());
        }
    }
}

mod sealed {
    use bevy::prelude::*;
    use bevy::render::render_resource::{AsBindGroup, ShaderRef};
    use bevy::sprite::{AlphaMode2d, Material2d};

    use crate::PrettyText;
    use crate::glyph::{GlyphOf, GlyphSpanEntity, GlyphSystems};

    use super::{
        DEFAULT_GLYPH_SHADER_HANDLE, DynMaterialRegistry, DynamicTextMaterial,
        ErasedPrettyTextMaterial, PrettyTextMaterial, PrettyTextMaterialPlugin, TextMaterial2d,
    };

    pub(super) fn plugin(app: &mut App) {
        app.add_plugins(PrettyTextMaterialPlugin::<DefaultGlyphMaterial>::default())
            .add_systems(
                PostUpdate,
                default_material.in_set(GlyphSystems::PropagateMaterial),
            )
            .add_observer(insert_erased_materials)
            .register_type::<Material>()
            .register_type::<DefaultGlyphMaterial>();
    }

    // Currently the type id is not used, but there could be some clever
    // error reporting here when multiple effects are added ¯\_(ツ)_/¯
    #[derive(Debug, Clone, Component, Reflect)]
    pub(super) struct Material(std::any::TypeId);

    impl Material {
        pub fn new<T: 'static>() -> Self {
            Self(std::any::TypeId::of::<T>())
        }
    }

    pub(super) fn default_material(
        mut commands: Commands,
        unmaterialized_text: Query<
            (Entity, Option<&Text2d>, Option<&TextSpan>),
            (
                With<PrettyText>,
                Without<Material>,
                Or<(Added<Text2d>, Added<TextSpan>, Added<PrettyText>)>,
            ),
        >,
        mut materials: ResMut<Assets<DefaultGlyphMaterial>>,
    ) {
        for (entity, text2d, span) in unmaterialized_text.iter() {
            if text2d.is_some_and(|text| !text.0.is_empty())
                || span.is_some_and(|text| !text.0.is_empty())
            {
                commands.entity(entity).insert(PrettyTextMaterial(
                    materials.add(DefaultGlyphMaterial::default()),
                ));
            }
        }
    }

    pub(super) fn apply_material<T: TextMaterial2d>(
        mut commands: Commands,
        glyphs: Query<
            (Entity, &GlyphSpanEntity),
            (
                With<GlyphOf>,
                Without<Material>,
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

    pub(super) fn register_dyn_material<T: Default + DynamicTextMaterial>(
        tag: &'static str,
    ) -> impl Fn(ResMut<DynMaterialRegistry>) {
        move |mut registry| {
            registry.insert(tag, Box::new(T::default()));
        }
    }

    fn insert_erased_materials(
        trigger: Trigger<OnAdd, ErasedPrettyTextMaterial>,
        mut commands: Commands,
        server: Res<AssetServer>,
        materials: Query<&ErasedPrettyTextMaterial>,
        registry: Res<DynMaterialRegistry>,
    ) -> Result {
        let material = materials.get(trigger.target())?;
        let handler = registry.get(material.tag.as_ref()).ok_or_else(|| {
            format!(
                "failed to insert text material: `{}` is not registered",
                material.tag
            )
        })?;

        let mut commands = commands.entity(trigger.target());
        handler.insert_from_args(material.args.as_ref(), &mut commands, &server)?;
        commands.remove::<ErasedPrettyTextMaterial>();

        Ok(())
    }

    #[derive(Default, Clone, Asset, AsBindGroup, Reflect)]
    pub(super) struct DefaultGlyphMaterial {
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

    #[cfg(feature = "proc-macro")]
    impl quote::ToTokens for ErasedPrettyTextMaterial {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            use quote::TokenStreamExt;
            let tag = &self.tag;
            let args = &self.args;
            tokens.append_all(quote::quote! {
                bevy_pretty_text::material::erased::ErasedPrettyTextMaterial::Tag {
                    tag: #tag.into(),
                    args: vec![#(#args.into(),)*]
                }
            });
        }
    }
}

#[cfg(test)]
mod test {
    use bevy::prelude::*;
    use bevy::render::render_resource::AsBindGroup;
    use bevy::sprite::Material2d;

    use crate::dynamic_effects::PrettyTextEffect;
    use crate::material::{PrettyTextMaterial, PrettyTextMaterialAppExt};
    use crate::parser::{Modifier, Modifiers};
    use crate::test::{prepare_app_with, run, run_tests};

    use super::{DynamicTextMaterial, TextMaterial2d};

    #[derive(Default, Clone, TypePath, AsBindGroup, Asset)]
    struct Material {
        atlas: Handle<Image>,
    }

    impl Material2d for Material {}

    impl TextMaterial2d for Material {
        fn set_atlas(&mut self, atlas: Handle<Image>) {
            self.atlas = atlas;
        }
    }

    impl DynamicTextMaterial for Material {
        fn insert_from_args(
            &self,
            args: &[std::borrow::Cow<'static, str>],
            entity: &mut EntityCommands,
            server: &AssetServer,
        ) -> Result<()> {
            assert_eq!(args.len(), 2);
            entity.insert(PrettyTextMaterial(server.add(Material::default())));
            Ok(())
        }
    }

    #[test]
    fn insert_material() {
        run_tests(
            || {
                let mut app = prepare_app_with(|app| {
                    app.register_pretty_material::<Material>("material");
                });
                app.world_mut().run_schedule(PreStartup);
                app.world_mut().flush();
                app
            },
            |app, entity, str| {
                app.world_mut()
                    .entity_mut(entity)
                    .insert(Modifiers(vec![Modifier::Effect(PrettyTextEffect {
                        tag: "material".into(),
                        args: vec!["1".into(), "2".into()],
                    })]));

                let has_spans = app
                    .world()
                    .entity(entity)
                    .get_components::<&Children>()
                    .is_some();

                app.world_mut().run_schedule(PostUpdate);
                app.world_mut().flush();
                run(
                    app,
                    move |effect: Query<&PrettyTextMaterial<Material>>,
                          glyphs: Query<&MeshMaterial2d<Material>>| {
                        assert!(
                            effect.single().is_ok(),
                            "expected 1, got {}",
                            effect.iter().len()
                        );

                        if !has_spans {
                            assert_eq!(
                                glyphs.iter().count(),
                                str.chars().count(),
                                "expected {}, got {}",
                                str.chars().count(),
                                glyphs.iter().len()
                            );
                        } else {
                            // spans should not inherit root effects
                            assert_eq!(
                                glyphs.iter().count(),
                                0,
                                "expected 0, got {}",
                                glyphs.iter().len(),
                            );
                        }
                    },
                );

                app.world_mut().entity_mut(entity).despawn();
                run(
                    app,
                    |effect: Query<&PrettyTextMaterial<Material>>,
                     glyphs: Query<&MeshMaterial2d<Material>>| {
                        assert!(effect.is_empty(), "expected 0, got {}", effect.iter().len());
                        assert!(glyphs.is_empty(), "expected 0, got {}", glyphs.iter().len());
                    },
                );
            },
        );
    }
}
