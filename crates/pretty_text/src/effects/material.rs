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
//! # use bevy_pretty_text::prelude::*;
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
//! [See defining custom effects.](crate::effects::dynamic#defining-custom-effects)
//!
//! # Implementation Note
//!
//! In [`Text`] hierarchies, the [`GlyphSpan`](crate::glyph::GlyphSpan) contains the
//! [`TextGlyphMaterial`](crate::ui_pipeline::TextGlyphMaterial) and its
//! [`Glyph`](crate::glyph::Glyph)s are batched in the [`ui_pipeline`](crate::ui_pipeline).
//!
//! In [`Text2d`] hierarchies, each [`Glyph`](crate::glyph::Glyph) entity contains
//! its own [`Mesh`] and [`MeshMaterial2d`].

use std::marker::PhantomData;

use bevy::asset::weak_handle;
use bevy::prelude::*;
use bevy::render::extract_component::ExtractComponent;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::sprite::AlphaMode2d;
use bevy::sprite::{Material2d, Material2dPlugin};

use crate::PrettyText;
use crate::effects::dynamic::{DynamicEffect, PrettyTextEffectAppExt, TrackedSpan};
use crate::glyph::{GlyphSystems, SpanAtlasImage};
use crate::render::GlyphMaterialPlugin;
use crate::style::Styles;

use super::EffectQuery;

pub(super) fn plugin(app: &mut App) {
    #[cfg(not(test))]
    {
        use bevy::asset::load_internal_asset;
        load_internal_asset!(
            app,
            DEFAULT_GLYPH_SHADER_HANDLE,
            "../default_glyph_material.wgsl",
            Shader::from_wgsl
        );
    }

    app.add_plugins((
        Material2dPlugin::<DefaultGlyphMaterial>::default(),
        GlyphMaterialPlugin::<DefaultGlyphMaterial>::default(),
    ))
    .add_systems(
        PostUpdate,
        (
            style_spans_default,
            update_material_atlas::<DefaultGlyphMaterial>,
            finish_propgation,
            #[cfg(debug_assertions)]
            multiple_materials_error,
            // Workaround for bevyengine/bevy#19048, ensuring the mesh components are
            // present before extraction-relevant systems.
            ApplyDeferred,
        )
            .chain()
            .before(bevy::sprite::check_entities_needing_specialization::<DefaultGlyphMaterial>)
            .in_set(GlyphSystems::PropagateMaterial),
    )
    .register_type::<DefaultGlyphMaterial>()
    .register_type::<PrettyTextMaterial<DefaultGlyphMaterial>>();
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
#[derive(Debug, Default, Clone, Component, ExtractComponent, Reflect)]
#[require(PrettyText)]
pub struct PrettyTextMaterial<M: GlyphMaterial>(pub Handle<M>);

/// A special material that renders [`Glyph`](crate::glyph::Glyph)s.
///
/// This trait should be derived . Implementors must additionally derive [`DynamicEffect`].
///
/// See [`Material2d`] and [`UiMaterial`] for information about `Bevy` materials.
///
/// See [the module documentation](crate::effects::material) for general information
/// about text materials and how to implement your own.
pub trait GlyphMaterial: Material2d + UiMaterial {
    /// Assigns this material's atlas.
    fn set_atlas(&mut self, atlas: Handle<Image>);
}

/// Derive macro for implementing [`GlyphMaterial`](pretty_text::material::GlyphMaterial)
/// and [`DynamicEffect`](pretty_text::material::DynamicEffect).
///
/// See [here](crate::effects::dynamic#material-effects) for an example of a material effect.
pub use pretty_text_macros::GlyphMaterial;

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
                style_spans::<T>.before(style_spans_default),
                update_material_atlas::<T>.before(finish_propgation),
                // Workaround for bevyengine/bevy#19048, ensuring the mesh components are
                // present before extraction-relevant systems.
                ApplyDeferred,
            )
                .chain()
                .before(bevy::sprite::check_entities_needing_specialization::<T>)
                .in_set(GlyphSystems::PropagateMaterial),
        )
        .register_type::<PrettyTextMaterial<T>>();
    }
}

#[derive(Component)]
struct AwaitingMaterialAsset<T>(PhantomData<T>);

fn style_spans<T: GlyphMaterial>(
    mut commands: Commands,
    effects: EffectQuery<&PrettyTextMaterial<T>>,
    mut spans: Query<
        (Entity, &mut Materials, Has<PrettyTextMaterial<T>>),
        (
            With<Styles>,
            Or<(
                Changed<Styles>,
                With<PropogateMaterial>,
                With<AwaitingMaterialAsset<T>>,
            )>,
        ),
    >,
    mut materials: ResMut<Assets<T>>,
) {
    for (span, mut span_materials, has_material) in spans.iter_mut() {
        let mut iter = effects.iter(span);
        let material = iter.next();
        if !has_material && let Some(handle) = material {
            #[cfg(debug_assertions)]
            if iter.next().is_some() {
                error!(
                    "Text span has multiple `{}` materials.",
                    std::any::type_name::<T>()
                );
            }

            let Some(material) = materials.get(&handle.0).cloned() else {
                commands
                    .entity(span)
                    .insert(AwaitingMaterialAsset::<T>(PhantomData));
                continue;
            };
            commands.entity(span).remove::<AwaitingMaterialAsset<T>>();
            let span_material = materials.add(material);

            span_materials.insert::<T>();
            commands
                .entity(span)
                .insert(PrettyTextMaterial(span_material.clone()));
        } else if material.is_none() {
            span_materials.remove::<T>();
            commands
                .entity(span)
                .remove::<(PrettyTextMaterial<T>, AwaitingMaterialAsset<T>)>();
        }
    }
}

fn style_spans_default(
    mut commands: Commands,
    spans: Query<
        (
            Entity,
            Has<PrettyTextMaterial<DefaultGlyphMaterial>>,
            &Materials,
        ),
        Or<(Changed<Styles>, With<PropogateMaterial>)>,
    >,
    mut materials: ResMut<Assets<DefaultGlyphMaterial>>,
) -> Result {
    for (span, has_material, span_materials) in spans.iter() {
        let materials_empty = span_materials.is_empty();
        if has_material && !materials_empty {
            commands
                .entity(span)
                .remove::<PrettyTextMaterial<DefaultGlyphMaterial>>();
        } else if !has_material && materials_empty {
            let span_material = materials.add(DefaultGlyphMaterial::default());
            commands
                .entity(span)
                .insert(PrettyTextMaterial(span_material.clone()));
        }
    }

    Ok(())
}

fn update_material_atlas<T: GlyphMaterial>(
    text: Query<
        (&PrettyTextMaterial<T>, &SpanAtlasImage),
        Or<(
            Changed<SpanAtlasImage>,
            Changed<PrettyTextMaterial<T>>,
            With<PropogateMaterial>,
            With<AwaitingMaterialAsset<T>>,
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

#[derive(Component)]
pub(crate) struct PropogateMaterial;

fn finish_propgation(mut commands: Commands, spans: Query<Entity, With<PropogateMaterial>>) {
    for entity in spans.iter() {
        commands.entity(entity).remove::<PropogateMaterial>();
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

#[derive(Default, Component)]
pub(crate) struct Materials {
    #[cfg(not(debug_assertions))]
    count: usize,
    #[cfg(debug_assertions)]
    ids: Vec<&'static str>,
}

impl Materials {
    fn insert<T: 'static>(&mut self) {
        #[cfg(not(debug_assertions))]
        {
            self.count += 1;
        }

        #[cfg(debug_assertions)]
        self.ids.push(std::any::type_name::<T>());
    }

    fn remove<T: 'static>(&mut self) {
        #[cfg(not(debug_assertions))]
        {
            self.count = self.count.saturating_sub(1);
        }

        #[cfg(debug_assertions)]
        self.ids.retain(|ty| *ty != std::any::type_name::<T>());
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn len(&self) -> usize {
        #[cfg(not(debug_assertions))]
        return self.count;

        #[cfg(debug_assertions)]
        self.ids.len()
    }
}

#[cfg(debug_assertions)]
#[derive(Component)]
struct MultipleMaterialError;

#[cfg(debug_assertions)]
fn multiple_materials_error(
    mut commands: Commands,
    spans: Query<(Entity, &Materials, Option<&TrackedSpan>), Without<MultipleMaterialError>>,
    remove_error: Query<Entity, (With<MultipleMaterialError>, Changed<Materials>)>,
) {
    for entity in remove_error.iter() {
        commands.entity(entity).remove::<MultipleMaterialError>();
    }

    for (entity, materials, tracked) in spans.iter() {
        if materials.len() > 1 {
            commands.entity(entity).insert(MultipleMaterialError);
            if let Some(tracked) = tracked {
                error!(
                    "Text span contains multiple materials: {:?}\n\
                    created: {}\n\
                    Only 1 material can be rendered at a time. \
                    This will cause unexpected behavior!",
                    materials.ids,
                    tracked.location(),
                );
            } else {
                error!(
                    "Text span contains multiple materials: {:?}\n\
                    Only 1 material can be rendered at a time. \
                    This will cause unexpected behavior!",
                    materials.ids,
                );
            }
        }
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
