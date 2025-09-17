//! Provides a collection of appearance effects.
//!
//! See [effects](super) for more information.

use bevy::prelude::*;

mod fadein;
mod scramble;
mod spread;

pub use fadein::*;
pub use scramble::*;
pub use spread::*;

use super::PrettyEffectSet;

pub(super) fn plugin(app: &mut bevy::prelude::App) {
    fadein::plugin(app);
    scramble::plugin(app);
    spread::plugin(app);

    app.add_systems(Update, tick_appeared.before(PrettyEffectSet));
}

/// Inserted into a revealed [`Glyph`].
///
/// A [`Glyph`] is revealed by the [`GlyphRevealed`] or [`WordRevealed`] events
/// produced by the [`Typewriter`].
///
/// Stores the duration in seconds since the glyph has appeared.
///
/// [`Glyph`]: crate::glyph::Glyph
/// [`GlyphRevealed`]: crate::typewriter::GlyphRevealed
/// [`WordRevealed`]: crate::typewriter::WordRevealed
/// [`Typewriter`]: crate::typewriter::Typewriter
#[derive(Debug, Default, Component, PartialEq)]
pub struct Appeared(pub f32);

fn tick_appeared(time: Res<Time>, mut appeared: Query<&mut Appeared>) {
    let delta = time.delta_secs();
    for mut appeared in appeared.iter_mut() {
        appeared.0 += delta;
    }
}
