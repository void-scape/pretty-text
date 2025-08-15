//! Provides a collection of behavioural effects.
//!
//! See [effects](super) for more information.

mod bounce;
mod breathe;
mod glitch;
mod pivot;
mod rainbow;
mod shake;
mod spin;
mod wave;
mod wobble;

pub use bounce::*;
pub use breathe::*;
pub use glitch::*;
pub use pivot::*;
pub use rainbow::*;
pub use shake::*;
pub use spin::*;
pub use wave::*;
pub use wobble::*;

pub(super) fn plugin(app: &mut bevy::prelude::App) {
    app.add_plugins((
        bounce::plugin,
        breathe::plugin,
        glitch::plugin,
        pivot::plugin,
        rainbow::plugin,
        shake::plugin,
        spin::plugin,
        wave::plugin,
        wobble::plugin,
    ));
}
