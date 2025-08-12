//! Provides a collection of behavioural effects.
//!
//! See [effects](super) for more information.

mod breathe;
mod glitch;
mod pivot;
mod rainbow;
mod shake;
mod spin;
mod wave;
mod wobble;

pub use breathe::*;
pub use glitch::*;
pub use pivot::*;
pub use rainbow::*;
pub use shake::*;
pub use spin::*;
pub use wave::*;
pub use wobble::*;

pub(super) fn plugin(app: &mut bevy::prelude::App) {
    breathe::plugin(app);
    glitch::plugin(app);
    pivot::plugin(app);
    rainbow::plugin(app);
    shake::plugin(app);
    spin::plugin(app);
    wave::plugin(app);
    wobble::plugin(app);
}
