//! Material [`effects`](crate::parser#effects) are `Bevy` [assets](bevy::asset)
//! that are dynamically constructed at run time and applied to text spans.
//!
//! Material effects refer to shader driven effects, such as [`Rainbow`]. For ECS
//! driven effects, see [the example here](crate::effects::dynamic#ecs-effects).
//!
//! [`Rainbow`]: crate::effects::behavior::Rainbow
//!
//! Only 1 material effect will work at a time, whereas [dynamic effects](crate::effects::dynamic)
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
//! world.spawn(pretty!("[my glitchy text span](glitch)"));
//!
//! // Effect with arguments
//! world.spawn(pretty!("[my rainbow text span](rainbow(1, 0.5))"));
//! ```
//!
//! # Defining Custom Materials
//!
//! A [`GlyphMaterial`] provides a vertex and fragment shader for the render
//! pipeline. It must derive [`AsBindGroup`], just like Bevy materials, for passing
//! data to the GPU.
//!
//! [See here for an example.](crate::effects::dynamic#defining-custom-effects)

use std::marker::PhantomData;

use bevy::asset::weak_handle;
use bevy::prelude::*;
use bevy::render::extract_component::ExtractComponent;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};

use crate::PrettyText;
use crate::effects::dynamic::{DynamicEffect, PrettyTextEffectAppExt};
use crate::render::GlyphMaterialPlugin;

pub(super) fn plugin(_app: &mut App) {
    #[cfg(not(test))]
    {
        use bevy::asset::load_internal_asset;
        load_internal_asset!(
            _app,
            DEFAULT_GLYPH_SHADER_HANDLE,
            "../default_glyph_material.wgsl",
            Shader::from_wgsl
        );
    }
}

/// The default shader for [`Glyph`](crate::glyph::Glyph)s.
///
/// Custom text materials can use the default vertex or fragment shader if no
/// special behavior is required.
pub const DEFAULT_GLYPH_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("35d4f25c-eb2b-4f26-872f-ef666a76554e");

/// A special material used for rendering a [`Glyph`](crate::glyph::Glyph).
///
/// See [the module documentation](crate::effects::material) for general information
/// about glyph materials and how to implement your own.
#[derive(Debug, Default, Clone, Component, ExtractComponent, Reflect)]
#[require(PrettyText)]
pub struct PrettyTextMaterial<M: GlyphMaterial>(pub Handle<M>);

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

/// Extension trait for registering [text materials](crate::effects::material).
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
