//! Provides a [system param](SystemParam) for retrieving the text data pointed
//! to by [`Glyph`] entities.

use bevy::ecs::system::SystemParam;
use bevy::prelude::*;
use bevy::text::ComputedTextBlock;

use crate::glyph::{Glyph, GlyphOf};

/// Utility for reading the text data pointed to by a [`Glyph`] entity.
#[derive(Debug, SystemParam)]
pub struct GlyphReader<'w, 's> {
    computed: Query<'w, 's, &'static ComputedTextBlock>,
    glyphs: Query<'w, 's, (&'static Glyph, &'static GlyphOf)>,
}

impl<'w, 's> GlyphReader<'w, 's> {
    /// Retrieve the text data pointed to by a `glyph`.
    pub fn read(&self, glyph: Entity) -> Result<&str> {
        Ok(self.glyphs.get(glyph).map(|(glyph, glyph_of)| {
            self.computed.get(glyph_of.root()).map(|computed| {
                let text = &computed.buffer().lines[glyph.0.line_index].text();
                &text[glyph.0.byte_index..glyph.0.byte_index + glyph.0.byte_length]
            })
        })??)
    }
}
