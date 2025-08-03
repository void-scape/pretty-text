//! Provides a collection of behavioural effects.
//!
//! See [effects](super) for more information.

mod glitch;
mod rainbow;
mod shake;
mod wave;
mod wobble;

pub use glitch::*;
pub use rainbow::*;
pub use shake::*;
pub use wave::*;
pub use wobble::*;

pub(super) fn plugin(app: &mut bevy::prelude::App) {
    glitch::plugin(app);
    rainbow::plugin(app);
    shake::plugin(app);
    wave::plugin(app);
    wobble::plugin(app);
}
