pub mod glitch;
pub mod rainbow;
pub mod shake;
pub mod wave;
pub mod wobble;

pub fn plugin(app: &mut bevy::prelude::App) {
    glitch::plugin(app);
    rainbow::plugin(app);
    shake::plugin(app);
    wave::plugin(app);
    wobble::plugin(app);
}
