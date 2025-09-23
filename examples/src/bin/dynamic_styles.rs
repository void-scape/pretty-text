//! This example provides an implementation for dynamically applying styles
//! to text spans based on substring matching.

#![allow(unused)]

use bevy::{color::palettes::css::RED, prelude::*, text::TextSpanAccess};
use bevy_pretty_text::{
    prelude::*,
    style::{Style, Styles},
};

fn main() {
    App::default()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_systems(Startup, (camera, spawn_text))
        .add_observer(style_text_dynamically::<Text>)
        .add_observer(style_text_dynamically::<Text2d>)
        .add_observer(style_text_dynamically::<TextSpan>)
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

fn spawn_text(mut commands: Commands) {
    commands.spawn((
        PrettyStyle("highlight"),
        ApplyStyleToString::Caseless("hello, world!"),
        // Alternatively:
        // ApplyStyleToString::Cased("Hello, World!"),
        TextFont::from_font_size(52.0),
        TextColor(RED.into()),
    ));

    commands.spawn((
        Node {
            flex_direction: FlexDirection::Column,
            ..Default::default()
        },
        children![
            (Text::new("Hello, World!"), PrettyText, Styles::default()),
            pretty!("Hello, World!"),
            pretty!("Apply to substring: `Hello, World!` Wow, awesome!"),
        ],
    ));
}

#[derive(Component)]
enum ApplyStyleToString {
    Cased(&'static str),
    Caseless(&'static str),
}

fn style_text_dynamically<C: Component + TextSpanAccess>(
    trigger: Trigger<OnAdd, C>,
    mut text_styles: Query<(&C, &mut Styles)>,
    style_entities: Query<(&PrettyStyle, &ApplyStyleToString)>,
) {
    let Ok((text, mut styles)) = text_styles.get_mut(trigger.target()) else {
        return;
    };

    for (style, apply_to_string) in style_entities.iter() {
        match apply_to_string {
            ApplyStyleToString::Cased(str) => {
                if text.read_span() == *str {
                    styles.0.push(Style::from_tag(style.0));
                }
            }
            ApplyStyleToString::Caseless(str) => {
                if str.len() == text.read_span().len()
                    && text
                        .read_span()
                        .chars()
                        .map(|c| c.to_ascii_lowercase())
                        .eq(str.chars().map(|c| c.to_ascii_lowercase()))
                {
                    styles.0.push(Style::from_tag(style.0));
                }
            }
        }
    }
}
