use bevy::prelude::*;

mod glyph;
mod type_writer;

pub struct PrettyTextPlugin;

impl Plugin for PrettyTextPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((glyph::GlyphMeshPlugin, type_writer::TypeWriterPlugin));
    }
}

#[derive(Default, Component)]
#[require(glyph::GlyphCount, glyph::OrderedGlyphs)]
pub struct PrettyText;

pub mod prelude {
    pub use crate::glyph::*;
    pub use crate::type_writer::*;
}
