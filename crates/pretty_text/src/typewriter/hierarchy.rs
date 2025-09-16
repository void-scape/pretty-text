//! See [`parser`](crate::parser#type-writer-syntax) for parsing type writer
//! hierarchy components.
//!
//! # Components
//!
//! The [`Typewriter`](super::Typewriter)'s sequence is configured in the
//! children hierarchy.
//!
//! Type writers iterate over their children, processing entities with special
//! sequencing components. In addition to Bevy's [`TextSpan`], this module provides
//! three additional sequencing components:
//! - [`TypewriterCommand`]
//! - [`TypewriterEvent`]
//! - [`TypewriterCallback`]

use std::sync::Arc;

use bevy::prelude::*;

/// A command processed by [`Typewriter`](super::Typewriter).
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # use bevy_pretty_text::typewriter::hierarchy::*;
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     Typewriter::new(30.0),
///     pretty!("normal speed <2>doubled speed"),
/// ));
///
/// // The `pretty` invocation above will expand to:
/// world.spawn((
///     Typewriter::new(30.0),
///     Text::default(),
///     children![
///         TextSpan::new("normal speed "),
///         TypewriterCommand::Speed(2.0),
///         TextSpan::new("doubled speed"),
///     ]
/// ));
/// ```
#[derive(Debug, Clone, Copy, Component, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub enum TypewriterCommand {
    /// Apply a multiplier to the base speed.
    Speed(f32),

    /// Pause for a duration in seconds.
    Pause(f32),
}

/// An event emitted by [`Typewriter`](super::Typewriter).
///
/// `TypewriterEvent` is both triggered and emitted, meaning that it can be received by an
/// [`Observer`] and an [`EventReader`].
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # use bevy_pretty_text::typewriter::hierarchy::*;
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     Typewriter::new(30.0),
///     pretty!("first span {my_event}second span"),
/// ));
///
/// // The `pretty` invocation above will expand to:
/// world.spawn((
///     Typewriter::new(30.0),
///     Text::default(),
///     children![
///         TextSpan::new("first span "),
///         TypewriterEvent::new("my_event"),
///         TextSpan::new("second span"),
///     ],
/// ));
/// ```
#[derive(Debug, Clone, Component, Event, Deref, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct TypewriterEvent(pub String);

impl AsRef<str> for TypewriterEvent {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl TypewriterEvent {
    /// Creates a new type writer event with a tag.
    #[inline]
    pub fn new(tag: impl Into<String>) -> Self {
        Self(tag.into())
    }
}

/// A [one shot system] triggered by [`Typewriter`](super::Typewriter).
///
/// [one shot system]: https://github.com/bevyengine/bevy/blob/2bddbdfd7c920d1ea61245dcdb7ff1c155e6b03b/examples/ecs/one_shot_systems.rs
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_pretty_text::prelude::*;
/// # use bevy_pretty_text::typewriter::hierarchy::*;
#[doc = include_str!("../../docs/audio_player.txt")]
/// # let mut world = World::new();
/// // Basic usage.
/// world.spawn((
///     Typewriter::new(30.0),
///     pretty!(
///         "sound has not played... {}sound has played!",
///         play_sound,
///     ),
/// ));
///
/// // The `pretty` invocation above will expand to:
/// world.spawn((
///     Typewriter::new(30.0),
///     Text::default(),
///     children![
///         TextSpan::new("sound has not played... "),
///         TypewriterCallback::new(play_sound),
///         TextSpan::new("sound has played!")
///     ],
/// ));
///
/// fn play_sound(mut commands: Commands, server: Res<AssetServer>) {
///     commands.spawn(AudioPlayer::new(
///         server.load("my-callback.ogg"),
///     ));
/// }
/// ```
#[derive(Clone, Component, Reflect)]
#[reflect(opaque)]
// Uses an Arc here because reflect(opaque) will not work with a Box.
pub struct TypewriterCallback(Arc<dyn Callback>);

impl core::fmt::Debug for TypewriterCallback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TypewriterCallback").finish_non_exhaustive()
    }
}

// Implements default for skipping in `ParsedPrettyText`.
impl Default for TypewriterCallback {
    fn default() -> Self {
        Self(Arc::new(|_: &mut World| {}))
    }
}

impl TypewriterCallback {
    /// Create a new callback with a bevy system.
    #[inline]
    pub fn new<M>(callback: impl IntoSystem<(), (), M> + Clone + Send + Sync + 'static) -> Self {
        Self(Arc::new(move |world: &mut World| {
            let _ = world.run_system_cached(callback.clone());
        }))
    }

    /// Create a new callback with mutable [`World`] access.
    #[inline]
    pub fn new_with(callback: impl Fn(&mut World) + Clone + Send + Sync + 'static) -> Self {
        Self(Arc::new(callback))
    }

    /// Queue the callback to run.
    #[inline]
    pub fn queue(&self, commands: &mut Commands) {
        self.0.queue(commands);
    }
}

dyn_clone::clone_trait_object!(Callback);
trait Callback: dyn_clone::DynClone + Send + Sync + 'static {
    fn queue(&self, commands: &mut Commands);
}

impl<F> Callback for F
where
    F: Fn(&mut World) + Clone + Send + Sync + 'static,
{
    #[inline]
    fn queue(&self, commands: &mut Commands) {
        commands.queue(self.clone());
    }
}
