//! Provides a collection of appearance effects.
//!
//! See [effects](super) for more information.

use bevy::prelude::*;
use bevy::render::view::VisibilitySystems;
use bevy_pretty_text::glyph::Glyph;

use crate::glyph::GlyphReader;
use crate::style::PrettyStyleSet;

mod scramble;
mod spread;

pub use scramble::*;
pub use spread::*;

use super::tween::TweenSet;

pub(super) fn plugin(app: &mut bevy::prelude::App) {
    scramble::plugin(app);
    spread::plugin(app);

    app.add_systems(
        PostUpdate,
        appeared
            .after(VisibilitySystems::VisibilityPropagate)
            .after(PrettyStyleSet)
            .before(TweenSet),
    );
}

/// Inserted into a revealed [`Glyph`].
///
/// A [`Glyph`] is considered revealed when its visibility components
/// ([`Visibility`], [`InheritedVisibility`]) become visible.
#[derive(Debug, Component)]
pub struct Appeared;

fn appeared(
    mut commands: Commands,
    glyphs: Query<
        (Entity, &Visibility, &InheritedVisibility),
        (
            Or<(
                Added<Glyph>,
                Changed<InheritedVisibility>,
                Changed<Visibility>,
            )>,
            With<Glyph>,
        ),
    >,
    reader: GlyphReader,
) -> Result {
    for (glyph, visibility, inherited_visibility) in glyphs.iter() {
        if reader.read(glyph)?.chars().all(char::is_whitespace) {
            continue;
        }

        match visibility {
            Visibility::Visible => {
                commands.entity(glyph).remove::<Appeared>().insert(Appeared);
            }
            Visibility::Hidden => {
                commands.entity(glyph).remove::<Appeared>();
            }
            Visibility::Inherited => match inherited_visibility.get() {
                true => {
                    commands.entity(glyph).remove::<Appeared>().insert(Appeared);
                }
                false => {
                    commands.entity(glyph).remove::<Appeared>();
                }
            },
        }
    }

    Ok(())
}
