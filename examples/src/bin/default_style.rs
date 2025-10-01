//! This example demonstrates how to apply a style to all `pretty!` text
//! automatically.
// TODO: Change description

use bevy::prelude::*;
use bevy_pretty_text::parser::Root;
use bevy_pretty_text::prelude::*;
use bevy_pretty_text::style::StyleWriter;

fn main() {
    App::default()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_systems(Startup, (camera, spawn_text))
        .add_systems(
            PostUpdate,
            (apply_default_style::<Text>, apply_default_style::<Text2d>).before(PrettyStyleSystems),
        )
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn spawn_text(mut commands: Commands) {
    // Define my default style
    commands.spawn((PrettyStyle("my_default_style"), effects![Pivot::default()]));

    // This text does not have a `Pivot` or point to `my_default_style`
    //
    // but the `apply_default_style` system will automatically apply the style
    // and this text will `Pivot`.
    commands.spawn((
        pretty2d!("Some normal text"),
        TextFont::from_font_size(52.0),
    ));
}

// TODO: This does not work if there is text in the root. Maybe the root
// should also have its own `Styles`? In that case, then `StyleWriter` would
// also need to be able to iterate over the root entity.
fn apply_default_style<R: Root>(
    roots: Query<Entity, Added<R>>,
    mut style_writer: StyleWriter<R>,
) -> Result {
    for root in roots.iter() {
        style_writer.iter_spans_mut(root)?.push("my_default_style");
    }
    Ok(())
}
