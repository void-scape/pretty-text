pub mod scramble;

pub fn plugin(app: &mut bevy::prelude::App) {
    scramble::plugin(app);
}
