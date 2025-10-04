use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::default()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .add_systems(Startup, (camera, spawn_text))
        .add_systems(Update, (case1_font, case1_transform))
        .add_systems(Update, (case2_font, case2_transform))
        .run();
}

fn camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}

#[derive(Component)]
struct Case1Span;

#[derive(Component)]
struct Case1Root;

#[derive(Component)]
struct Case2Span;

#[derive(Component)]
struct Case2Root;

fn spawn_text(mut commands: Commands) {
    commands.spawn((
        Text2d::default(),
        Case1Root,
        children![(TextSpan::new("Case 1"), Case1Span)],
        PrettyText,
        Wave::default(),
    ));

    commands.spawn((
        Text::default(),
        Case2Root,
        children![(TextSpan::new("Case 2"), Case2Span)],
        PrettyText,
        Wave::default(),
    ));
}

fn case1_font(input: Res<ButtonInput<KeyCode>>, mut case1: Single<&mut TextFont, With<Case1Span>>) {
    if input.just_pressed(KeyCode::Digit1) {
        // case1.font_size += 5.0;
    }
}

fn case1_transform(
    input: Res<ButtonInput<KeyCode>>,
    mut case1: Single<&mut Transform, With<Case1Root>>,
) {
    if input.just_pressed(KeyCode::Digit1) {
        case1.scale += Vec3::splat(0.1);
    }
}

fn case2_font(input: Res<ButtonInput<KeyCode>>, mut case2: Single<&mut TextFont, With<Case2Span>>) {
    if input.just_pressed(KeyCode::Digit2) {
        // case2.font_size += 5.0;
    }
}

fn case2_transform(
    input: Res<ButtonInput<KeyCode>>,
    mut case2: Single<&mut UiTransform, With<Case2Root>>,
) {
    if input.just_pressed(KeyCode::Digit2) {
        case2.scale += Vec2::splat(0.2);
    }
}
