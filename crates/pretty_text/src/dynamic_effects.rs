//! **Dynamic effects** are normal rust types that are dynamically constructed at
//! run time and inserted into text hierarchies.
//!
//! Dynamic effects refer to ECS driven effects, such as `shake`. For shader
//! effects, see [`materials`](crate::material).
//!
//! # Parser Syntax
//!
//! **Spans** are ranges of text, denoted with backticks: ``"`...`"``.
//!
//! **Modifiers** are a comma separated collection of effects and styles, which
//! directly follow a **span** and are contained in square brackets: `"[mod1, ...]"`.
//!
//! **Effects** are a modifier that optional take arguments.
//!
//! See [`parser`](crate::parser).
//!
//! ## Examples
//!
//! ``"`I am a span`[my_effect]"``
//!
//! ``"`I am a span`[my_effect(10, 4.3), another_effect]"``
//!
//! # Using Dynamic Effects
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::*;
//! #
//! # let mut world = World::new();
//! // Built-in effects are provided with the `default_effects` feature!
//! world.spawn(pretty!("`my shaky text span`[shake]"));
//! world.spawn(pretty!("`my wavy text span`[wave]"));
//! // ...
//!
//! // Default arguments example
//!
//! #[derive(Default)]
//! struct Shake {
//!     arg1: usize,
//!     arg2: f32,
//! }
//!
//! world.spawn(
//!     pretty!("`my shaky text span`[shake(10)]"),
//! //                    arg2 is defaulted! -^
//! );
//!
//! // This is just syntax sugar for:
//! Shake {
//!     arg1: 10,
//!     ..Default::default()
//! }
//! ```
//!
//! # Defining Custom Effects
//!
//! To position glyphs, use [`GlyphOrigin`](crate::glyph::GlyphOrigin) and
//! [`GlyphOffset`](crate::glyph::GlyphOffset). Ensure that updates to the
//! `GlyphOffset` occur in the [`FixedUpdate`] schedule before the
//! [`PrettyTextSystems`](crate::PrettyTextSystems) system set.
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::*;
//! #
//! # let mut app = App::default();
//! // Defining a custom effect.
//! #[derive(Component, TextEffect)]
//! #[require(PrettyText)]
//! struct MyEffect;
//!
//! // Registering `MyEffect`.
//! app.register_pretty_effect::<MyEffect>("my_effect");
//!
//! # let mut world = World::new();
//! // Using `MyEffect`.
//! world.spawn(pretty!("`my text span`[my_effect]"));
//! ```

use std::borrow::Cow;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::material::{DynMaterialRegistry, ErasedPrettyTextMaterial};
use crate::parser::{Modifier, Modifiers};

/// Extension trait for registering [dynamic effects](crate::dynamic_effects).
pub trait PrettyTextEffectAppExt {
    /// Register effect `T` with a `tag`.
    fn register_pretty_effect<T: Default + DynamicEffect>(
        &mut self,
        tag: &'static str,
    ) -> &mut Self;
}

impl PrettyTextEffectAppExt for App {
    fn register_pretty_effect<T: Default + DynamicEffect>(
        &mut self,
        tag: &'static str,
    ) -> &mut Self {
        self.add_systems(
            PreStartup,
            move |mut registry: ResMut<DynEffectRegistry>| {
                registry.insert(tag, Box::new(T::default()));
            },
        )
    }
}

/// Constructs `Self` from `args` and inserts into an entity.
///
/// See [`dynamic_effects`](crate::dynamic_effects).
///
/// This trait should be derived with [`TextEffect`](pretty_text_macros::TextEffect).
pub trait DynamicEffect: Send + Sync + 'static {
    /// Construct a dynamic effect from `args` and insert into `entity`.
    ///
    /// Returns a [`BevyError`] if the effect can not constructed from `args`.
    fn insert_from_args(
        &self,
        args: &[Cow<'static, str>],
        entity: &mut EntityCommands,
    ) -> Result<()>;
}

/// A dynamic representation of a text effect.
///
/// Used by [`bevy_pretty_text::parser`] to dynamically insert text effects.
#[derive(Debug, Clone)]
pub struct PrettyTextEffect {
    /// Tag associated to a [registered dynamic effect](PrettyTextEffectAppExt).
    pub tag: Cow<'static, str>,

    /// Field arguments for a dynamic effect.
    pub args: Vec<Cow<'static, str>>,
}

/// Dynamic effect registry.
///
/// See [`dynamic_effects`](crate::dynamic_effects).
#[derive(Default, Deref, DerefMut, Resource)]
pub struct DynEffectRegistry(pub HashMap<&'static str, Box<dyn DynamicEffect>>);

impl std::fmt::Debug for DynEffectRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DynEffectRegistry")
            .field(&self.0.keys())
            .finish()
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for PrettyTextEffect {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;
        let tag = &self.tag;
        let args = &self.args;
        tokens.append_all(quote::quote! {
            bevy_pretty_text::dynamic_effects::PrettyTextEffect {
                tag: std::borrow::Cow::Borrowed(#tag),
                args: vec![#(std::borrow::Cow::Borrowed(#args),)*]
            }
        });
    }
}

pub(crate) fn text_effect(
    trigger: Trigger<OnAdd, Modifiers>,
    mut commands: Commands,
    material_registry: Res<DynMaterialRegistry>,
    effects_registry: Res<DynEffectRegistry>,
    mods: Query<&Modifiers>,
) -> Result {
    let mods = mods.get(trigger.target())?;

    let mut material = None;
    for effect in mods.0.iter().filter_map(|m| match m {
        Modifier::Effect(effect) => Some(effect),
        _ => None,
    }) {
        if material_registry.0.get(effect.tag.as_ref()).is_some() {
            if let Some(other) = material {
                return Err(format!(
                    "registered multiple materials on a single span: `{}` and `{}`",
                    other, effect.tag
                )
                .into());
            }

            material = Some(&effect.tag);
            commands
                .entity(trigger.target())
                .insert(ErasedPrettyTextMaterial {
                    tag: effect.tag.clone(),
                    args: effect.args.clone(),
                });
        } else if let Some(handler) = effects_registry.get(effect.tag.as_ref()) {
            handler.insert_from_args(&effect.args, &mut commands.entity(trigger.target()))?;
        } else {
            error!("effect `{}` is not registered", effect.tag);
        }
    }

    Ok(())
}
