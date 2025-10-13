//! [Dynamic effects](crate::parser#effect-shorthand) are normal rust types that are
//! dynamically constructed at run time and applied to text spans.
//!
//! Dynamic effects can be ECS driven, such as [`Shake`](crate::effects::behavior::Shake),
//! or [shader driven](super::material), such as [`Rainbow`](crate::effects::behavior::Rainbow).
//!
//! # Using Dynamic Effects
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! # let mut world = World::new();
//! // Built-in effects are provided with the `default_effects` feature!
//! world.spawn(pretty!("[my shaky text span](shake)"));
//!
//! // Effect with arguments.
//! world.spawn(pretty!("[my wavy text span](wave(1, 0.5))"));
//! ```
//!
//! # Defining Custom Effects
//!
//! ## ECS effects
//!
//! Effects apply transformations to [`GlyphVertices`]. Transformations are accumulated
//! and must be performed every frame in the [`Main`] schedule.
//!
//! The position of a [`Glyph`](crate::glyph::Glyph) relative to the text block is stored in
//! [`PositionedGlyph::position`](bevy::text::PositionedGlyph), which is wrapped
//! by the [`Glyph`](crate::glyph::Glyph) component. The scale of a [`Glyph`](crate::glyph::Glyph)
//! relative to the text root and font size is stored in [`GlyphScale`](crate::glyph::GlyphScale).
//!
//! Effects should scale their parameters by the [`GlyphScale`](crate::glyph::GlyphScale)
//! when applicable.
//!
//! [`Glyph`]: crate::glyph::Glyph
//! [`GlyphVertices`]: crate::glyph::GlyphVertices
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! // Defining a custom effect.
//! #[derive(Default, Component, Reflect, DynamicEffect)]
//! #[require(PrettyText)]
//! struct MyEffect {
//!     field1: f32,
//!     field2: usize,
//! }
//!
//! # let mut app = App::default();
//! // Registering `MyEffect`.
//! app.register_pretty_effect::<MyEffect>("my_effect");
//!
//! # let mut world = World::new();
//! // Using `MyEffect`.
//! world.spawn(pretty!("[my text span](my_effect)"));
//!
//! // Using `MyEffect` with arguments.
//! world.spawn(pretty!("[my text span](my_effect(1))"));
//! world.spawn(pretty!("[my text span](my_effect(1, 1))"));
//! ```
//!
//! ## Material effects
//!
//! See [`material`](crate::effects::material) for more information.
//!
//! ```no_run
//! # use bevy::prelude::*;
//! # use bevy_pretty_text::prelude::*;
//! # use bevy::shader::ShaderRef;
//! # use bevy::render::render_resource::AsBindGroup;
//! # use bevy::sprite_render::Material2d;
//! #[derive(Clone, Asset, AsBindGroup, Reflect, DynamicEffect)]
//! // Tells `DynamicEffect` to treat `MyMaterial` as a material asset.
//! #[pretty_text(material)]
//! pub struct MyMaterial {
//!     #[uniform(0)]
//!     pub intensity: f32,
//!
//!     #[uniform(1)]
//!     pub radius: f32,
//! }
//!
//! // Provide default values for the derived `DynamicEffect`.
//! impl Default for MyMaterial {
//!     fn default() -> Self {
//!         Self {
//!             intensity: 0.02,
//!             radius: 4.0,
//!         }
//!     }
//! }
//!
//! impl GlyphMaterial for MyMaterial {
//!     fn vertex_shader() -> ShaderRef {
//!         // Write a custom vertex shader
//!         "my_shader.wgsl".into()
//!     }
//!
//!     fn fragment_shader() -> ShaderRef {
//!         // Write a custom fragment shader
//!         "my_shader.wgsl".into()
//!     }
//! }
//!
//! # let mut app = App::default();
//! // Registering `MyEffect`.
//! //
//! // Registering a material will allow you to use them in the parser.
//! app.register_pretty_material::<MyMaterial>("my_material");
//!
//!
//! # let mut world = World::new();
//! // Using `MyMaterial`.
//! world.spawn(pretty!("[my text span](my_material)"));
//!
//! // Using `MyMaterial` with arguments.
//! world.spawn(pretty!("[my text span](my_material(1))"));
//! world.spawn(pretty!("[my text span](my_material(1, 1))"));
//! ```

pub use pretty_text_parser::syntax::{
    Argument, DynamicEffectFieldSyntax, DynamicEffectSyntax, GetDynamicEffectSyntax,
    ReflectGetDynamicEffectSyntax, get_dynamic_effect_syntax,
};

use std::panic::Location;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::style::Arg;

/// Extension trait for registering [dynamic effects](crate::effects::dynamic).
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
/// This trait should not be implemented manually and instead derived.
///
/// See [here](crate::effects::dynamic#ecs-effects) for an example.
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

/// Derive macro for implementing [`DynamicEffect`].
///
/// See [here](crate::effects::dynamic#ecs-effects) for an example of an ECS effect.
///
/// See [here](crate::effects::dynamic#material-effects) for an example of a material effect.
pub use pretty_text_macros::DynamicEffect;

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
    /// [`GetDynamicEffectSyntax`].
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

/// Dynamic effect registry.
///
/// See [the module documentation](crate::effects::dynamic).
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
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Component, Reflect, DynamicEffect)]
    struct TestEffect {
        intensity: f32,
        speed: f32,
        name: String,
        enabled: bool,
    }

    impl Default for TestEffect {
        fn default() -> Self {
            Self {
                intensity: 1.0,
                speed: 0.5,
                name: "default".to_string(),
                enabled: true,
            }
        }
    }

    fn app() -> App {
        let mut app = App::new();
        app.add_plugins((MinimalPlugins, AssetPlugin::default()))
            .init_resource::<DynEffectRegistry>();
        app.finish();
        app
    }

    #[test]
    fn registration() {
        let mut app = app();

        app.register_pretty_effect::<TestEffect>("effect");
        app.update();

        let registry = app.world().resource::<DynEffectRegistry>();
        assert!(registry.get("effect").is_some());
    }

    fn insert_effect(args: &[Arg]) -> Result<TestEffect, DynamicEffectError> {
        let mut app = app();
        app.register_pretty_effect::<TestEffect>("effect");
        app.update();

        let entity = app.world_mut().spawn_empty().id();

        let registry = std::mem::take(app.world_mut().resource_mut::<DynEffectRegistry>().as_mut());
        let effect_handler = registry.get("effect").unwrap();

        let type_registry =
            std::mem::take(app.world_mut().resource_mut::<AppTypeRegistry>().as_mut());

        effect_handler
            .insert_from_args(
                &type_registry,
                &app.world().resource::<AssetServer>().clone(),
                &mut app.world_mut().commands().entity(entity),
                args,
            )
            .map(|_| {
                app.world_mut().flush();
                app.world().get::<TestEffect>(entity).unwrap().clone()
            })
    }

    #[test]
    fn insert_effect_no_args() {
        let effect = insert_effect(&[]).unwrap();
        assert_eq!(effect, TestEffect::default());
    }

    #[test]
    fn insert_effect_positional_args() {
        let effect = insert_effect(&[
            Arg::Positioned("2.5".into()),
            Arg::Positioned("1.5".into()),
            Arg::Positioned("\"custom_name\"".into()),
        ])
        .unwrap();
        assert_eq!(effect.intensity, 2.5);
        assert_eq!(effect.speed, 1.5);
        assert_eq!(effect.name, "custom_name");
        assert_eq!(effect.enabled, true);
    }

    #[test]
    fn insert_effect_named_args() {
        let effect = insert_effect(&[
            Arg::Named {
                field: "intensity".into(),
                value: "3.0".into(),
            },
            Arg::Named {
                field: "enabled".into(),
                value: "false".into(),
            },
            Arg::Named {
                field: "name".into(),
                value: "\"named_effect\"".into(),
            },
        ])
        .unwrap();
        assert_eq!(effect.intensity, 3.0);
        assert_eq!(effect.speed, 0.5);
        assert_eq!(effect.name, "named_effect");
        assert_eq!(effect.enabled, false);
    }

    #[test]
    fn insert_effect_mixed_args() {
        let effect = insert_effect(&[
            Arg::Positioned("4.0".into()),
            Arg::Named {
                field: "name".into(),
                value: "\"mixed_effect\"".into(),
            },
        ])
        .unwrap();
        assert_eq!(effect.intensity, 4.0);
        assert_eq!(effect.speed, 0.5);
        assert_eq!(effect.name, "mixed_effect");
        assert_eq!(effect.enabled, true);
    }

    #[test]
    fn insert_effect_errors() {
        {
            let err = insert_effect(&[
                Arg::Positioned("1.0".into()),
                Arg::Positioned("2.0".into()),
                Arg::Positioned("name".into()),
                Arg::Positioned("true".into()),
                Arg::Positioned("extra".into()),
            ])
            .unwrap_err();
            matches!(err.kind(), ErrorKind::TooManyArgs { .. });
        }

        {
            let err = insert_effect(&[Arg::Named {
                field: "invalid_field".into(),
                value: "value".into(),
            }])
            .unwrap_err();
            matches!(err.kind(), ErrorKind::InvalidNamedArg { .. });
        }

        {
            let err = insert_effect(&[Arg::Positioned("not_a_number".into())]).unwrap_err();
            matches!(err.kind(), ErrorKind::Parser { .. });
        }

        {
            let err = insert_effect(&[
                Arg::Named {
                    field: "intensity".into(),
                    value: "1.0".into(),
                },
                Arg::Positioned("2.0".into()),
            ])
            .unwrap_err();
            matches!(err.kind(), ErrorKind::InvalidPositionalArg { .. });
        }
    }
}
