//! Provides a collection of appearance effects.
//!
//! See [effects](super) for more information.

use bevy::prelude::*;

mod scramble;
mod spread;

pub use scramble::*;
pub use spread::*;

use super::PrettyEffectSet;

pub(super) fn plugin(app: &mut bevy::prelude::App) {
    scramble::plugin(app);
    spread::plugin(app);

    app.add_systems(Update, tick_appeared.before(PrettyEffectSet));
}

/// Inserted into a revealed [`Glyph`].
///
/// A [`Glyph`] is revealed when its [`GlyphIndex`](crate::glyph::GlyphIndex)
/// is contained withinin the [`Reveal`](crate::typewriter::Reveal) range.
///
/// Stores the duration in seconds since the glyph has appeared.
///
/// [`Glyph`]: crate::glyph::Glyph
#[derive(Debug, Default, Component, PartialEq)]
pub struct Appeared(f32);

fn tick_appeared(time: Res<Time>, mut appeared: Query<&mut Appeared>) {
    let delta = time.delta_secs();
    for mut appeared in appeared.iter_mut() {
        appeared.0 += delta;
    }
}
