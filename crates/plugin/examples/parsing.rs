use bevy::color::palettes::css::{LIGHT_BLUE, RED};
use bevy::prelude::*;
use bevy_pretty_text::prelude::*;
use pretty_text_effects::wave::Wave;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_systems(
            Startup,
            (
                |mut commands: Commands| {
                    commands.spawn(Camera2d);
                },
                dynamic_parse,
            ),
        )
        .register_pretty_style("custom_style", |_| {
            PrettyStyle::new(
                TextFont {
                    font_size: 32.,
                    ..Default::default()
                },
                LIGHT_BLUE,
            )
        })
        .run();
}

fn dynamic_parse(
    mut commands: Commands,
    // server: Res<AssetServer>,
    // mut styles: ResMut<PrettyStyleRegistry>,
) -> Result {
    let font = TextFont {
        font_size: 48.,
        ..Default::default()
    };

    let custom_style_font = TextFont {
        font_size: 32.,
        ..Default::default()
    };

    // styles.register(
    //     "custom_style",
    //     PrettyStyle::new(custom_style_font.clone(), BLUE),
    // );

    commands.spawn((
        pretty!("`hello|custom_style`[wavy] world`!|red`"),
        font.clone(),
        Transform::from_xyz(0., 150., 0.),
    ));

    commands.spawn((
        PrettyTextParser::parse("`hello|custom_style`[wavy] world`!|red`")?,
        font.clone(),
        Transform::from_xyz(-150., -150., 0.),
    ));

    commands.spawn((
        PrettyText,
        Text2d::default(),
        font.clone(),
        children![
            (
                TextSpan::new("hello"),
                Wave::default(),
                TextColor(LIGHT_BLUE.into()),
                custom_style_font
            ),
            (TextSpan::new(" world"), font.clone()),
            (TextSpan::new("!"), TextColor(RED.into()), font)
        ],
        Transform::from_xyz(150., -150., 0.),
    ));

    Ok(())
}
