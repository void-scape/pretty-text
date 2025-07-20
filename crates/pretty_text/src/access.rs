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

#[cfg(test)]
mod test {
    // This test currently fails for wide glyphs due to an upstream issue.

    // use bevy::prelude::*;
    //
    // use crate::glyph::Glyphs;
    // use crate::test::{prepare_app, run, run_tests};
    //
    // use super::GlyphReader;
    //
    // #[test]
    // fn glyph_reader() {
    //     run_tests(prepare_app, |app, _, str| {
    //         app.world_mut().run_schedule(PostUpdate);
    //         app.world_mut().flush();
    //         run(app, move |reader: GlyphReader, root: Single<&Glyphs>| {
    //             let repro = root
    //                 .iter()
    //                 .map(|glyph| reader.read(glyph).unwrap())
    //                 .collect::<String>();
    //
    //             assert_eq!(
    //                 repro.chars().count(),
    //                 str.chars().count(),
    //                 "failed with: \"{}\", read as \"{}\"",
    //                 str,
    //                 repro
    //             );
    //         });
    //     });
    // }
}
