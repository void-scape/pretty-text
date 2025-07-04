use bevy::prelude::*;

pub extern crate pretty_text_effects as effects;

pub mod prelude {
    pub use super::PrettyTextPlugin;
    pub use pretty_text::PrettyText;
    pub use pretty_text::glyph::*;
    pub use pretty_text::material::*;
    pub use pretty_text::parser::*;
    pub use pretty_text::style::*;
    pub use pretty_text::type_writer::*;
    pub use pretty_text_macros::pretty;
}

pub use pretty_text::PrettyText;
pub use pretty_text::access;
pub use pretty_text::glyph;
pub use pretty_text::material;
pub use pretty_text::parser;
pub use pretty_text::style;
pub use pretty_text::type_writer;
pub use pretty_text_macros::pretty;

pub struct PrettyTextPlugin;

impl Plugin for PrettyTextPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            pretty_text::PrettyTextCorePlugin,
            pretty_text_effects::EffectsPlugin,
        ));
    }
}
