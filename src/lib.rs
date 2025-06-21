use bevy::prelude::*;

mod glyph;
mod reveal;

pub struct PrettyTextPlugin;

impl Plugin for PrettyTextPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((glyph::GlyphMeshPlugin, reveal::RevealPlugin));
    }
}

pub mod prelude {
    pub use crate::glyph::*;
    pub use crate::reveal::*;
}
