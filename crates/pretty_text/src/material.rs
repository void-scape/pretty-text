//! Material [`effects`](crate::parser#effects) are `Bevy` [assets](bevy::asset)
//! that are dynamically constructed at run time and inserted into text hierarchies.
//!
//! Material effects refer to shader driven effects, such as `glitch`. For ECS
//! driven effects, see [the example here](crate::dynamic_effects#ecs-effects).
//!
//! For [`Text`] and [`Text2d`] hierarchies, materials are applied with
//! [`TextGlyphMaterial`](crate::ui_pipeline::TextGlyphMaterial) and
//! [`MeshMaterial2d`] components on [`Glyph`](crate::glyph::Glyph) entities.
//! This means that only 1 material effect will work at a time, whereas
//! [`dynamic_effects`](crate::dynamic_effects) can be layered.
//!
//! If no material is supplied, then the [default glyph material] is used.
//!
//! [default glyph material]: DEFAULT_GLYPH_SHADER_HANDLE
//!
//! # Using Material Effects
//!
//! ```
//! # use bevy::prelude::*;
#![doc = include_str!("../docs/pretty.txt")]
//! #
//! # let mut world = World::new();
//! // Built-in effects are provided with the `default_effects` feature!
//! world.spawn(pretty!("`my glitchy text span`[glitch]"));
//!
//! // Effect with arguments
//! world.spawn(pretty!("`my rainbow text span`[rainbow(1, 0.5)]"));
//! ```
//!
//! # Defining Custom Materials
//!
//! Material effects are normal `Bevy` [`Material2d`] and [`UiMaterial`] types
//! that contain a special `atlas` field. The `atlas` field is a handle to the
//! glyph atlas asset which allows glyphs to directly sample from the glyph atlas
//! in the fragment shader.
//!
//! The glyph meshes are packed with extra vertex data to define the position and
//! size of a glyph in the glyph atlas texture.
//!
//! ```ignore
#![doc = include_str!("../docs/material.txt")]
//! ```
//!
//! # Implementation Note
//!
//! In [`Text`] hierarchies, the [`GlyphSpanEntity`] contains the
//! [`TextGlyphMaterial`](crate::ui_pipeline::TextGlyphMaterial) and its
//! [`Glyph`](crate::glyph::Glyph)s are  batched in the [`ui_pipeline`](crate::ui_pipeline).
//!
//! In [`Text2d`] hierarchies, each [`Glyph`](crate::glyph::Glyph) entity contains
//! its own [`MeshMaterial2d`].

use std::marker::PhantomData;

use bevy::asset::weak_handle;
use bevy::prelude::*;
use bevy::sprite::{Material2d, Material2dPlugin};

use crate::PrettyText;
use crate::dynamic_effects::{DynamicEffect, PrettyTextEffectAppExt};
use crate::glyph::{GlyphSystems, SpanAtlasImage};
use crate::ui_pipeline::GlyphMaterialPlugin;

pub(super) fn plugin(app: &mut App) {
    app.add_plugins(PrettyTextMaterialPlugin::<DefaultGlyphMaterial>::default())
        .add_systems(
            PostUpdate,
            default_material.in_set(GlyphSystems::PropagateMaterial),
        )
        .register_type::<Material>()
        .register_type::<DefaultGlyphMaterial>();
}

/// The default shader for [`Glyph`](crate::glyph::Glyph)s.
///
/// Custom text materials can use the default vertex or fragment shader if no
/// special behavior is required.
pub const DEFAULT_GLYPH_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("35d4f25c-eb2b-4f26-872f-ef666a76554e");

/// A special material used for rendering a [`Glyph`](crate::glyph::Glyph).
///
/// See [`Material2d`] and [`UiMaterial`] for information about `Bevy` materials.
///
/// See [`material`](crate::material) for general information about text materials
/// and how to implement your own.
#[derive(Debug, Default, Clone, Component, Reflect)]
#[require(PrettyText, Material::new::<Self>())]
pub struct PrettyTextMaterial<M: GlyphMaterial>(pub Handle<M>);

/// A special material that renders [`Glyph`](crate::glyph::Glyph)s.
///
/// This trait should be derived with [`GlyphMaterial`]. Implementors should
/// additionally derive [`DynamicEffect`].
///
/// See [`Material2d`] and [`UiMaterial`] for information about `Bevy` materials.
///
/// See [`material`](crate::material) for general information about text materials
/// and how to implement your own.
///
/// [`GlyphMaterial`]:
/// https://docs.rs/bevy_pretty_text/latest/bevy_pretty_text/derive.GlyphMaterial.html
/// [`DynamicEffect`]:
/// https://docs.rs/bevy_pretty_text/latest/bevy_pretty_text/derive.DynamicEffect.html
pub trait GlyphMaterial: Material2d + UiMaterial {
    /// Assigns this material's atlas.
    fn set_atlas(&mut self, atlas: Handle<Image>);
}

/// Extension trait for registering [text materials](crate::material).
pub trait PrettyTextMaterialAppExt {
    /// Register material `T` with a `tag`.
    fn register_pretty_material<T>(&mut self, tag: &'static str) -> &mut Self
    where
        T: Default + GlyphMaterial + DynamicEffect,
        T::Data: PartialEq + Eq + std::hash::Hash + Clone;
}

impl PrettyTextMaterialAppExt for App {
    fn register_pretty_material<T>(&mut self, tag: &'static str) -> &mut Self
    where
        T: Default + GlyphMaterial + DynamicEffect,
        T::Data: PartialEq + Eq + std::hash::Hash + Clone,
    {
        self.add_plugins(PrettyTextMaterialPlugin::<T>::default())
            .register_pretty_effect::<T>(tag)
    }
}

/// Adds the necessary ECS resources and render logic to enable rendering entities
/// using the given [`GlyphMaterial`] asset type.
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
    T: GlyphMaterial,
    T::Data: PartialEq + Eq + std::hash::Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.add_plugins((
            Material2dPlugin::<T>::default(),
            GlyphMaterialPlugin::<T>::default(),
        ))
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
                .in_set(GlyphSystems::PropagateMaterial),
        )
        .register_type::<PrettyTextMaterial<T>>();
    }
}

/// Propagate the glyph atlas handle from span entities to glyph entities.
pub fn set_material_atlas<T: GlyphMaterial>(
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

use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::AlphaMode2d;

use crate::glyph::{GlyphOf, GlyphSpanEntity, Text2dGlyph, TextGlyph};
use crate::ui_pipeline::TextGlyphMaterial;

#[derive(Debug, Clone, Component, Reflect)]
struct Material(&'static str);

impl Material {
    pub fn new<T: 'static>() -> Self {
        Self(std::any::type_name::<T>())
    }
}

fn default_material(
    mut commands: Commands,
    unmaterialized_text: Query<
        Entity,
        (
            With<PrettyText>,
            Without<Material>,
            Or<(
                Added<Text>,
                Added<Text2d>,
                Added<TextSpan>,
                Added<PrettyText>,
            )>,
        ),
    >,
    mut materials: ResMut<Assets<DefaultGlyphMaterial>>,
) {
    for entity in unmaterialized_text.iter() {
        commands.entity(entity).insert(PrettyTextMaterial(
            materials.add(DefaultGlyphMaterial::default()),
        ));
    }
}

fn apply_material<T: GlyphMaterial>(
    mut commands: Commands,
    glyphs: Query<
        (Entity, &GlyphSpanEntity, Has<TextGlyph>, Has<Text2dGlyph>),
        (
            With<GlyphOf>,
            Without<Material>,
            Without<PrettyTextMaterial<T>>,
            Or<(With<TextGlyph>, With<Text2dGlyph>)>,
        ),
    >,
    spans: Query<(&PrettyTextMaterial<T>, Has<TextGlyphMaterial<T>>)>,
) {
    for (entity, span_entity, is_text_glyph, is_text2d_glyph) in glyphs.iter() {
        if is_text_glyph && is_text2d_glyph {
            error!(
                "`Glyph` is marked as a `TextGlyph` and `Text2dGlyph`. \
                    This will cause unexpected behavior!"
            );
        }

        if let Ok((material, has_glyph_material)) = spans.get(span_entity.0) {
            if is_text2d_glyph {
                debug_assert!(!has_glyph_material);
                commands
                    .entity(entity)
                    .insert(MeshMaterial2d(material.0.clone()));
            } else {
                debug_assert!(is_text_glyph);
                if !has_glyph_material {
                    commands
                        .entity(span_entity.0)
                        .insert(TextGlyphMaterial(material.0.clone()));
                }
            }
        }
    }
}

#[derive(Default, Clone, Asset, AsBindGroup, Reflect)]
struct DefaultGlyphMaterial {
    #[texture(0)]
    #[sampler(1)]
    atlas: Handle<Image>,
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

// The ui pipeline defaults to `DEFAULT_GLYPH_SHADER_HANDLE`
impl UiMaterial for DefaultGlyphMaterial {}

impl GlyphMaterial for DefaultGlyphMaterial {
    fn set_atlas(&mut self, atlas: Handle<Image>) {
        self.atlas = atlas;
    }
}

#[cfg(test)]
mod test {
    use bevy::prelude::*;
    use bevy::render::render_resource::AsBindGroup;
    use bevy::sprite::Material2d;

    use crate::dynamic_effects::DynamicEffectResult;
    use crate::material::{PrettyTextMaterial, PrettyTextMaterialAppExt};
    use crate::modifier::{Arg, Modifier, Modifiers};
    use crate::test::{prepare_app_with, run, run_tests};

    use super::{DynamicEffect, GlyphMaterial};

    #[derive(Default, Clone, TypePath, AsBindGroup, Asset)]
    struct Material {
        atlas: Handle<Image>,
    }

    impl Material2d for Material {}
    impl UiMaterial for Material {}

    impl GlyphMaterial for Material {
        fn set_atlas(&mut self, atlas: Handle<Image>) {
            self.atlas = atlas;
        }
    }

    impl DynamicEffect for Material {
        fn insert_from_args(
            &self,
            _registry: &AppTypeRegistry,
            server: &AssetServer,
            entity: &mut EntityCommands,
            args: &[Arg],
        ) -> DynamicEffectResult {
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
                    .insert(Modifiers(vec![Modifier {
                        tag: "material".into(),
                        args: vec!["1".into(), "2".into()],
                    }]));

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
