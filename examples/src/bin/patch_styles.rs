//! This example demonstrates how to change styles dynamically at run time
//! both manually and with the `StyleWriter` system param.
// TODO: example broken??

use bevy::color::palettes::css::RED;
use bevy::prelude::*;
use bevy_pretty_text::prelude::*;
use prettytext_examples::BalatroPlugin;

fn main() {
    App::default()
        .add_plugins((BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, (spawn_style, spawn_text))
        .run();
}

#[derive(Component)]
struct ManualStyle;

fn spawn_style(mut commands: Commands, server: Res<AssetServer>) {
    // Here, I make a style that will be changed manually.
    commands.spawn((
        PrettyStyle("manual"),
        // So I will mark it and query for it later.
        ManualStyle,
        effects![
            PrettyTextMaterial(server.add(Rainbow::default())),
            Wave::default(),
        ],
    ));

    // Here, I make a simple text style with 2 effects.
    commands.spawn((
        // The `StyleUiWriter` can act on the `PrettyStyle` tags, so I do not
        // need to mark it.
        PrettyStyle("pretty"),
        effects![
            PrettyTextMaterial(server.add(Rainbow::default())),
            Wave::default(),
        ],
    ));

    // And another that is red and glitchy.
    commands.spawn((
        PrettyStyle("scary"),
        TextColor(RED.into()),
        effects![PrettyTextMaterial(server.add(Glitch {
            intensity: 0.04,
            frequency: 200.0,
            ..Default::default()
        }))],
    ));
}

fn spawn_text(mut commands: Commands) {
    commands.spawn(Camera2d);

    // Text containers.
    let manual_container = commands
        .spawn(Node {
            width: Val::Percent(100f32),
            height: Val::Percent(50f32),
            top: Val::Percent(50f32),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            ..Default::default()
        })
        .id();
    let style_writer_container = commands
        .spawn(Node {
            width: Val::Percent(100f32),
            height: Val::Percent(50f32),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            ..Default::default()
        })
        .id();

    // Spawn the text that will be changed manually.
    commands
        .spawn((
            pretty!("[Don't pick me!](manual)"),
            TextFont {
                font_size: 42.0,
                ..Default::default()
            },
            ChildOf(manual_container),
        ))
        .observe(scary_manual)
        .observe(pretty_manual);

    // Spawn the text that will be changed with the `StyleUiWriter`.
    commands
        .spawn((
            pretty!("[Pick me!](scary)"),
            TextFont {
                font_size: 42.0,
                ..Default::default()
            },
            ChildOf(style_writer_container),
        ))
        .observe(scary_style_writer)
        .observe(pretty_style_writer);
}

fn pretty_style_writer(over: On<Pointer<Over>>, mut writer: StyleUiWriter) -> Result {
    // Iterate over all text spans from `root` and replace the `pretty` style
    // with the `scary` style.
    writer
        // This will fail if the provided entity does not have a `Text` component.
        .iter_spans_mut(over.event().entity)?
        .replace("scary", "pretty");

    Ok(())
}

fn scary_style_writer(out: On<Pointer<Out>>, mut writer: StyleUiWriter) -> Result {
    // Iterate over all text spans from `root` and replace the `scary` style
    // with the `pretty` style.
    writer
        // This will fail if the provided entity does not have a `Text` component.
        .iter_spans_mut(out.event().entity)?
        .replace("pretty", "scary");

    Ok(())
}

fn pretty_manual(
    _: On<Pointer<Out>>,
    mut commands: Commands,
    style: Single<Entity, With<ManualStyle>>,
    mut materials: ResMut<Assets<Rainbow>>,
) {
    // Replace the text components and styles.
    commands
        .entity(*style)
        .despawn_related::<Effects>()
        .remove::<TextColor>()
        .insert(effects![
            PrettyTextMaterial(materials.add(Rainbow::default())),
            Wave::default(),
        ]);
}

fn scary_manual(
    _: On<Pointer<Over>>,
    mut commands: Commands,
    style: Single<Entity, With<ManualStyle>>,
    mut materials: ResMut<Assets<Glitch>>,
) {
    // Replace the text components and styles.
    commands
        .entity(*style)
        .despawn_related::<Effects>()
        .insert((
            TextColor(RED.into()),
            effects![PrettyTextMaterial(materials.add(Glitch {
                intensity: 0.04,
                frequency: 200.0,
                ..Default::default()
            }))],
        ));
}
