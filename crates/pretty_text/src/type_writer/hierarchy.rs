//! # Type Writer Syntax
//!
//! See [`parser`](crate::parser) for an overview of the parsing functionality.
//!
//! The [`TypeWriter`](crate::type_writer::TypeWriter) has built-in syntax for
//! sequencing:
//! - Pause: `[<seconds>]`
//!     - ex: `"Pause[1] between"`
//! - Set relative speed: `<<mult>>`
//!     - ex: `"<2.0>Fast <0.2>Slow"`
//! - Emit [`TypeWriterEvent`]s: `{<my_event>}`
//!     - ex: `"Emit an {my_event}event"`
//!
//! And in the special case of the [`pretty`](pretty_text_macros::pretty) macro:
//! - Trigger [`TypeWriterCallback`]s: `{}`
//!     - ex: `pretty!("Trigger a {}callback", |mut commands: Commands| { ... })`
//!
//! # Components
//!
//! The [`TypeWriter`](super::TypeWriter)'s sequence is configured in the
//! children hierarchy.
//!
//! Type writers iterate over their children, processing entities with special
//! sequencing components. In addition to Bevy's [`TextSpan`], this module provides
//! three additional sequencing components:
//! - [`TypeWriterCommand`]
//! - [`TypeWriterEvent`]
//! - [`TypeWriterCallback`]

use bevy::prelude::*;

/// A command processed by [`TypeWriter`](super::TypeWriter).
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     TypeWriter::new(30.0),
///     Text2d::default(),
///     children![
///         TextSpan::new("normal speed"),
///         TypeWriterCommand::Speed(2.0),
///         TextSpan::new("doubled speed"),
///     ]
/// ));
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect)]
pub enum TypeWriterCommand {
    /// Apply a multiplier to the base speed.
    Speed(f32),

    /// Pause for a duration in seconds.
    Pause(f32),
}

/// An event emitted by [`TypeWriter`](super::TypeWriter).
///
/// `TypeWriterEvent` is both triggered and emitted, meaning that it can be received by an
/// [`Observer`] and an [`EventReader`].
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
/// # let mut world = World::new();
/// // Basic usage.
/// world
///     .spawn((
///         TypeWriter::new(30.0),
///         Text2d::default(),
///         children![
///             TextSpan::new("first span"),
///             TypeWriterEvent::new("my_event"),
///             TextSpan::new("second span"),
///         ],
///     ))
///     .observe(|trigger: Trigger<TypeWriterEvent>| {
///         assert_eq!(trigger.event(), "my_event");
///     });
/// ```
#[derive(Debug, Clone, Component, Event, Deref, Reflect)]
pub struct TypeWriterEvent(pub String);

impl AsRef<str> for TypeWriterEvent {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl TypeWriterEvent {
    /// Creates a new type writer event with a tag.
    pub fn new(tag: impl Into<String>) -> Self {
        Self(tag.into())
    }
}

/// A [one shot system] triggered by [`TypeWriter`](super::TypeWriter).
///
/// [one shot system]: https://github.com/bevyengine/bevy/blob/2bddbdfd7c920d1ea61245dcdb7ff1c155e6b03b/examples/ecs/one_shot_systems.rs
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::type_writer::*;
/// #
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     TypeWriter::new(30.0),
///     Text2d::default(),
///     children![
///         TextSpan::new("sound has not played..."),
///         TypeWriterCallback::new(play_sound),
///         TextSpan::new("sound has played!"),
///     ],
/// ));
///
/// fn play_sound(mut commands: Commands, server: Res<AssetServer>) {
///     commands.spawn(AudioBundle {
///         source: server.load("my-callback.ogg"),
///         ..default()
///     });
/// }
/// ```
#[derive(Clone, Component)]
pub struct TypeWriterCallback(Box<dyn Callback>);

impl core::fmt::Debug for TypeWriterCallback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TypeWriterCallback").finish_non_exhaustive()
    }
}

impl TypeWriterCallback {
    /// Create a new callback with a bevy system.
    pub fn new<M>(callback: impl IntoSystem<(), (), M> + Clone + Send + Sync + 'static) -> Self {
        Self(Box::new(move |world: &mut World| {
            let _ = world.run_system_cached(callback.clone());
        }))
    }

    /// Create a new callback with mutable [`World`] access.
    pub fn new_with(callback: impl Fn(&mut World) + Clone + Send + Sync + 'static) -> Self {
        Self(Box::new(callback))
    }

    /// Queue the callback to run.
    pub fn queue(&self, commands: &mut Commands) {
        self.0.queue(commands);
    }

    #[doc(hidden)]
    pub fn placeholder() -> Self {
        Self(Box::new(|_: &mut World| {}))
    }
}

dyn_clone::clone_trait_object!(Callback);
trait Callback: dyn_clone::DynClone + Send + Sync + 'static {
    // fn register(&)

    fn queue(&self, commands: &mut Commands);
}

impl<F> Callback for F
where
    F: Fn(&mut World) + Clone + Send + Sync + 'static,
{
    fn queue(&self, commands: &mut Commands) {
        commands.queue(self.clone());
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for TypeWriterCommand {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Speed(speed) => {
                quote::quote! { bevy_pretty_text::type_writer::hierarchy::TypeWriterCommand::Speed(#speed) }
            }
            Self::Pause(duration) => {
                quote::quote! { bevy_pretty_text::type_writer::hierarchy::TypeWriterCommand::Pause(#duration) }
            }
        });
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for TypeWriterEvent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        let event = &self.0;
        tokens.append_all(quote::quote! {
            bevy_pretty_text::type_writer::hierarchy::TypeWriterEvent(
                String::from(#event),
            )
        });
    }
}
