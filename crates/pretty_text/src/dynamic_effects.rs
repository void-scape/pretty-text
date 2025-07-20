//! Dynamic [`effects`](crate::parser#effects) are normal rust types that are
//! dynamically constructed at run time and inserted into text hierarchies.
//!
//! Dynamic effects refer to ECS driven effects, such as `shake`. For shader
//! effects, see [`materials`](crate::material).
//!
//! # Using Dynamic Effects
//!
//! ```
//! # use bevy::prelude::*;
//! # use pretty_text::*;
#![doc = include_str!("docs/pretty")]
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
//! };
//! ```
//!
//! # Defining Custom Effects
//!
//! To position glyphs, use [`GlyphOrigin`](crate::glyph::GlyphOrigin) and
//! [`GlyphOffset`](crate::glyph::GlyphOffset). Ensure that updates to the
//! [`GlyphOffset`](crate::glyph::GlyphOffset) occur in the [`Update`]
//! schedule before the [`GlyphSystems::Position`](crate::glyph::GlyphSystems::Position)
//! system set.
//!
//! ```ignore
#![doc = include_str!("../../docs_common/effect")]
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
                registry.register(tag, T::default());
            },
        )
    }
}

/// Constructs `Self` from `args` and inserts into an entity.
///
/// See [`dynamic_effects`](crate::dynamic_effects).
///
/// This trait should be derived with
/// [`DynamicEffect`](https://docs.rs/bevy_pretty_text/derive.DynamicEffect.html).
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
#[derive(Default, Resource)]
pub struct DynEffectRegistry(HashMap<&'static str, Box<dyn DynamicEffect>>);

impl std::fmt::Debug for DynEffectRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DynEffectRegistry")
            .field(&self.0.keys())
            .finish()
    }
}

impl DynEffectRegistry {
    /// Register an `effect` with `tag`.
    #[inline]
    pub fn register(&mut self, tag: &'static str, effect: impl DynamicEffect) {
        self.0.insert(tag, Box::new(effect));
    }

    /// Unregisters the effect with `tag`.
    #[inline]
    pub fn unregister(&mut self, tag: &'static str) {
        self.0.remove(tag);
    }

    /// Retrieves the effect registered with `tag`.
    #[inline]
    pub fn get(&self, tag: &str) -> Option<&dyn DynamicEffect> {
        self.0.get(tag).map(|mat| mat.as_ref())
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
        if material_registry.get(effect.tag.as_ref()).is_some() {
            if let Some(other) = material {
                return Err(format!(
                    "registered multiple materials on a single span: `{}` and `{}`",
                    other, effect.tag
                )
                .into());
            }

            material = Some(effect.tag.as_ref());
            commands
                .entity(trigger.target())
                .insert(ErasedPrettyTextMaterial {
                    tag: effect.tag.clone(),
                    args: effect.args.clone(),
                });
        } else if let Some(handler) = effects_registry.0.get(effect.tag.as_ref()) {
            handler.insert_from_args(&effect.args, &mut commands.entity(trigger.target()))?;
        } else {
            error!("effect `{}` is not registered", effect.tag);
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use bevy::prelude::*;

    use crate::dynamic_effects::PrettyTextEffect;
    use crate::glyph::Glyph;
    use crate::parser::{Modifier, Modifiers};
    use crate::test::{prepare_app, run, run_tests};

    use super::{DynamicEffect, PrettyTextEffectAppExt};

    #[derive(Default, Component)]
    struct Effect;

    impl DynamicEffect for Effect {
        fn insert_from_args(
            &self,
            args: &[std::borrow::Cow<'static, str>],
            entity: &mut EntityCommands,
        ) -> Result<()> {
            assert_eq!(args.len(), 2);
            entity.insert(Effect);
            Ok(())
        }
    }

    #[test]
    fn insert_effect() {
        run_tests(
            || {
                let mut app = prepare_app();
                app.register_pretty_effect::<Effect>("effect");
                app.world_mut().run_schedule(PreStartup);
                app.world_mut().flush();
                app
            },
            |app, entity, str| {
                app.world_mut()
                    .entity_mut(entity)
                    .insert(Modifiers(vec![Modifier::Effect(PrettyTextEffect {
                        tag: "effect".into(),
                        args: vec!["1".into(), "2".into()],
                    })]));

                app.world_mut().run_schedule(PostUpdate);
                app.world_mut().flush();
                run(app, move |effect: Query<&Effect>, glyphs: Query<&Glyph>| {
                    assert!(
                        effect.single().is_ok(),
                        "expected 1, got {}",
                        effect.iter().len()
                    );
                    assert_eq!(
                        glyphs.iter().len(),
                        // all glyph entities
                        str.chars().count(),
                        "expected {}, got {}",
                        str.chars().count(),
                        glyphs.iter().len()
                    );
                });

                app.world_mut().entity_mut(entity).despawn();
                run(app, |effect: Query<&Effect>, glyphs: Query<&Glyph>| {
                    assert!(effect.is_empty(), "expected 0, got {}", effect.iter().len());
                    assert!(glyphs.is_empty(), "expected 0, got {}", glyphs.iter().len());
                });
            },
        )
    }
}
