//! Material [`effects`](crate::parser#effects) are `Bevy` [assets](bevy::asset)
//! that are dynamically constructed at run time and inserted into text hierarchies.
//!
//! Material effects refer to shader driven effects, such as `rainbow`. For ECS
//! driven effects, see [the example here](crate::dynamic_effects#ecs-effects).
//!
//! Only 1 material effect will work at a time, whereas [`dynamic_effects`](crate::dynamic_effects)
//! can be composed.
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
//! A [`GlyphMaterial`] provides a vertex and fragment shader for the render
//! pipeline. It must derive [`AsBindGroup`], just like Bevy materials, for passing
//! data to the GPU.
//!
//! [See here for an example.](crate::effects::dynamic#defining-custom-effects)
//!
//! For more information, see the [`render`](crate::render) module.

use std::marker::PhantomData;

use bevy::asset::weak_handle;
use bevy::ecs::component::HookContext;
use bevy::ecs::world::DeferredWorld;
use bevy::prelude::*;
use bevy::render::extract_component::ExtractComponent;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};

use crate::PrettyText;
use crate::effects::dynamic::{DynamicEffect, PrettyTextEffectAppExt};
use crate::render::GlyphMaterialPlugin;

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

    app.add_plugins(GlyphMaterialPlugin::<DefaultGlyphMaterial>::default())
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
/// See [the module documentation](crate::effects::material) for general information
/// about glyph materials and how to implement your own.
#[derive(Debug, Default, Clone, Component, ExtractComponent, Reflect)]
#[require(PrettyText, ErasedMaterial)]
// #[component(on_remove = remove_erased)]
pub struct PrettyTextMaterial<M: GlyphMaterial>(pub Handle<M>);

fn remove_erased(mut world: DeferredWorld, ctx: HookContext) {
    world
        .commands()
        .entity(ctx.entity)
        .remove::<ErasedMaterial>();
}

/// A marker component for entities with [`PrettyTextMaterial`].
#[derive(Debug, Default, Component)]
pub struct ErasedMaterial;

/// A special material that renders [`Glyph`](crate::glyph::Glyph)s.
///
/// Implementors must additionally derive [`DynamicEffect`].
///
/// See [the module documentation](crate::effects::material) for general information
/// about glyph materials and how to implement your own.
pub trait GlyphMaterial: AsBindGroup + Asset + Clone + Sized {
    /// Returns this material's vertex shader. If [`ShaderRef::Default`] is returned,
    /// the default glyph vertex shader will be used.
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Default
    }

    /// Returns this material's fragment shader. If [`ShaderRef::Default`] is returned,
    /// the default glyph fragment shader will be used.
    fn fragment_shader() -> ShaderRef {
        ShaderRef::Default
    }
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
#[derive(Debug)]
pub struct PrettyTextMaterialPlugin<T>(PhantomData<T>);

impl<T> Default for PrettyTextMaterialPlugin<T> {
    fn default() -> Self {
        PrettyTextMaterialPlugin(PhantomData)
    }
}

impl<T> Plugin for PrettyTextMaterialPlugin<T>
where
    T: GlyphMaterial,
    T::Data: PartialEq + Eq + std::hash::Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.add_plugins(GlyphMaterialPlugin::<T>::default())
            .register_type::<PrettyTextMaterial<T>>();
    }
}

#[derive(Default, Clone, Asset, AsBindGroup, Reflect)]
pub struct DefaultGlyphMaterial {}

impl GlyphMaterial for DefaultGlyphMaterial {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(DEFAULT_GLYPH_SHADER_HANDLE)
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
