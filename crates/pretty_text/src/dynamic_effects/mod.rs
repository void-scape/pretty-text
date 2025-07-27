//! Dynamic [`effects`](crate::parser#effects) are normal rust types that are
//! dynamically constructed at run time and inserted into text hierarchies.
//!
//! Dynamic effects can be ECS driven, such as `shake`, or
//! [shader driven](crate::material), such as `rainbow`.
//!
//! # Using Dynamic Effects
//!
//! ```
//! # use bevy::prelude::*;
//! # use pretty_text::*;
#![doc = include_str!("../../docs/pretty.txt")]
//! #
//! # let mut world = World::new();
//! // Built-in effects are provided with the `default_effects` feature!
//! world.spawn(pretty!("`my shaky text span`[shake]"));
//!
//! // Effect with arguments.
//! world.spawn(pretty!("`my wavy text span`[wave(1, 0.5)]"));
//! ```
//!
//! # Defining Custom Effects
//!
//! ## ECS effects
//!
//! To position glyphs, use [`GlyphOrigin`](crate::glyph::GlyphOrigin) and
//! [`GlyphOffset`](crate::glyph::GlyphOffset). Ensure that updates to the
//! [`GlyphOffset`](crate::glyph::GlyphOffset) occur in the [`Update`]
//! schedule or in [`PostUpdate`] before the [`GlyphSystems::Position`]
//! system set.
//!
//! [`GlyphSystems::Position`]: crate::glyph::GlyphSystems::Position
//!
//! ```ignore
#![doc = include_str!("../../docs/effect.txt")]
//! ```
//!
//! ## Material effects
//!
//! See [`material`](crate::material) for more information.
//!
//! ```ignore
#![doc = include_str!("../../docs/material.txt")]
//! ```

use std::panic::Location;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::modifier::Arg;

use self::syntax::{DynamicEffectSyntax, get_dynamic_effect_syntax};

pub mod syntax;

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
    /// Returns a [`DynamicEffectResult`] if the effect can not be constructed from `args`.
    fn insert_from_args(
        &self,
        registry: &AppTypeRegistry,
        server: &AssetServer,
        entity: &mut EntityCommands,
        args: &[Arg],
    ) -> DynamicEffectResult;
}

/// Result of a [`DynamicEffect::insert_from_args`] operation.
pub type DynamicEffectResult = std::result::Result<(), DynamicEffectError>;

/// Error produced by a [`DynamicEffect`] when applied to an entity.
///
/// Contains location information, an [`ErrorKind`], and syntax documentation for
/// the [`DynamicEffect`].
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct DynamicEffectError(Box<DynamicEffectErrorInner>);

impl DynamicEffectError {
    /// Create a new [`DynamicEffectError`] given an effect that might implement
    /// [`GetDynamicEffectSyntax`](syntax::GetDynamicEffectSyntax).
    pub fn from_effect<T: Reflect>(
        registry: &AppTypeRegistry,
        effect: &T,
        kind: ErrorKind,
    ) -> Self {
        Self(Box::new(DynamicEffectErrorInner {
            syntax: get_dynamic_effect_syntax(registry, effect),
            location: None,
            kind,
        }))
    }

    /// Set the tracked `location` for this error.
    pub fn tracked(&mut self, location: &'static Location<'static>) {
        self.0.location = Some(location);
    }

    /// Get the [`ErrorKind`].
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }

    /// Get the [`Location`] associated with the source of this error.
    pub fn location(&self) -> Option<&'static Location<'_>> {
        self.0.location
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{}", self.fmt())]
struct DynamicEffectErrorInner {
    syntax: Option<DynamicEffectSyntax<'static>>,
    location: Option<&'static Location<'static>>,
    kind: ErrorKind,
}

impl DynamicEffectErrorInner {
    fn fmt(&self) -> String {
        let location = match self.location {
            Some(loc) => &format!("\ncreated: {loc}"),
            None => "",
        };
        let syntax = match self.syntax {
            Some(syntax) => &format!("\n{}", syntax.help_fmt()),
            None => "",
        };
        format!("{}{location}{syntax}", self.kind)
    }
}

/// Error produced by a [`DynamicEffect`] when applied to an entity.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ErrorKind {
    /// Failed to parse the argument.
    #[error("failed to parse `{arg}` for field `{field}` in `{effect}`:\n{error}")]
    Parser {
        /// The type name of the effect.
        effect: &'static str,
        /// The name of the field.
        field: &'static str,
        /// The supplied argument.
        arg: Arg,
        /// The parser error.
        error: String,
    },

    /// Too many positional arguments were supplied.
    #[error("too many arguments in `{effect}`")]
    TooManyArgs {
        /// The type name of the effect.
        effect: &'static str,
    },

    /// Positional argument was supplied after a named argument.
    #[error("cannot supply positional argument after a named argument in `{effect}`")]
    InvalidPositionalArg {
        /// The type name of the effect.
        effect: &'static str,
    },

    /// Named argument does not match any fields in the effect.
    #[error("effect `{effect}` has no field `{name}`")]
    InvalidNamedArg {
        /// Argument name.
        name: String,
        /// The type name of the effect.
        effect: &'static str,
    },

    /// Named argument overrides previously set value.
    #[error("named argument `{name}` overrides previous positional argument in `{effect}`")]
    NamedArgOverride {
        /// Argument name.
        name: String,
        /// The type name of the effect.
        effect: &'static str,
    },
}

/// Component for tracking the location where spans are spawned.
///
/// Used to report location in [`DynamicEffectError`].
#[derive(Debug, Clone, Copy, Component, Reflect)]
pub struct TrackedSpan(&'static Location<'static>);

impl Default for TrackedSpan {
    #[track_caller]
    fn default() -> Self {
        Self::new()
    }
}

impl TrackedSpan {
    /// Mark caller as the location that this span was created.
    #[track_caller]
    pub fn new() -> Self {
        Self(Location::caller())
    }

    /// Get the [`Location`] that this span was created.
    pub fn location(&self) -> &'static Location<'static> {
        self.0
    }
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
        if self.0.get(tag).is_some() {
            error!("effect `{tag}` is already registered");
        }

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

#[cfg(test)]
mod test {
    use bevy::prelude::*;

    use crate::glyph::Glyph;
    use crate::modifier::{Arg, Modifier, Modifiers};
    use crate::test::{prepare_app, run, run_tests};

    use super::{DynamicEffect, DynamicEffectResult, PrettyTextEffectAppExt};

    #[derive(Default, Component)]
    struct Effect;

    impl DynamicEffect for Effect {
        fn insert_from_args(
            &self,
            _registry: &AppTypeRegistry,
            _server: &AssetServer,
            entity: &mut EntityCommands,
            args: &[Arg],
        ) -> DynamicEffectResult {
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
                    .insert(Modifiers(vec![Modifier {
                        tag: "effect".into(),
                        args: vec!["1".into(), "2".into()],
                    }]));

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
