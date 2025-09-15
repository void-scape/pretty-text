use std::time::Duration;

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy::text::TextBounds;
use bevy_pretty_box::{Textbox, TextboxAdvance, TextboxContainer, TextboxContinue, TextboxName};
use bevy_seedling::prelude::*;

pub struct TextboxPlugin;

impl Plugin for TextboxPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, (scene, generate_triangle))
            .add_systems(Update, animate_triangle)
            .add_observer(textbox_container)
            .add_observer(textbox)
            .add_observer(textbox_name_add)
            .add_observer(textbox_name_remove)
            .add_observer(textbox_continue);
    }
}

fn scene(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn(Camera2d);
    commands.spawn((
        Name::new("Desk"),
        Sprite::from_image(server.load("paper-ui/desk.png")),
        Transform::from_xyz(0.0, 0.0, -1.0).with_scale(Vec3::splat(1.5)),
    ));
}

fn textbox_container(
    trigger: Trigger<OnAdd, TextboxContainer>,
    mut commands: Commands,
    server: Res<AssetServer>,
) {
    commands
        .entity(trigger.target())
        .insert(Transform::from_xyz(0.0, 25.0, 0.0))
        .with_child((
            Name::new("Textbox Sprite"),
            Sprite::from_image(server.load("paper-ui/paper.png")),
            Transform::from_scale(Vec3::splat(1.5)),
        ))
        .observe(
            |_: Trigger<TextboxAdvance>, mut commands: Commands, server: Res<AssetServer>| {
                commands.spawn(SamplePlayer::new(server.load("page-flip.ogg")));
            },
        );
}

fn textbox(trigger: Trigger<OnAdd, Textbox>, mut commands: Commands, server: Res<AssetServer>) {
    commands.entity(trigger.target()).insert((
        TextBounds::new_horizontal(378.0 * 2.0),
        TextFont {
            font_size: 48.0,
            font: server.load("canterbury/Canterbury.ttf"),
            ..Default::default()
        },
        TextColor(Color::BLACK),
        Anchor::TopLeft,
        Transform::from_xyz(-378.0, 0.0, 0.0),
    ));
}

#[derive(Component)]
struct NameComponents;

fn textbox_name_add(
    trigger: Trigger<OnInsert, TextboxName>,
    mut commands: Commands,
    server: Res<AssetServer>,
    name: Query<&TextboxName, With<Textbox>>,
) {
    let Ok(TextboxName(Some(name))) = name.get(trigger.target()) else {
        return;
    };

    commands.entity(trigger.target()).insert(children![(
        Transform::from_xyz(163.0, 132.0, 1.0),
        Visibility::default(),
        Name::new("Textbox Name"),
        children![
            (
                NameComponents,
                Text2d::new(name),
                TextFont {
                    font_size: 48.0,
                    font: server.load("canterbury/Canterbury.ttf"),
                    ..Default::default()
                },
                TextColor(Color::BLACK),
            ),
            (
                NameComponents,
                Sprite::from_image(server.load("paper-ui/speaker-header.png")),
                Transform::from_xyz(0.0, -9.0, -1.0).with_scale(Vec3::splat(1.5)),
            )
        ]
    )]);
}

fn textbox_name_remove(
    trigger: Trigger<OnRemove, TextboxName>,
    mut commands: Commands,
    name: Query<&Children, With<Textbox>>,
    name_components: Query<Entity, With<NameComponents>>,
) {
    let Ok(children) = name.get(trigger.target()) else {
        return;
    };

    for entity in name_components.iter_many(children) {
        commands.entity(entity).despawn();
    }
}

fn textbox_continue(
    trigger: Trigger<OnAdd, TextboxContinue>,
    mut commands: Commands,
    triangle: Res<TriangleMesh>,
) {
    commands.entity(trigger.target()).insert((
        Transform::from_xyz(360.0, -285.0, 1.0).with_scale(Vec3::splat(2.0)),
        Triangle {
            timer: Timer::new(Duration::from_secs_f32(0.5), TimerMode::Repeating),
            up: false,
        },
        triangle.mesh(),
    ));
}

#[derive(Resource)]
struct TriangleMesh {
    mesh: Handle<Mesh>,
    material: Handle<ColorMaterial>,
}

impl TriangleMesh {
    pub fn mesh(&self) -> impl Bundle {
        (
            Mesh2d(self.mesh.clone()),
            MeshMaterial2d(self.material.clone()),
        )
    }
}

fn generate_triangle(
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut commands: Commands,
) {
    let triangle_size = 10.0;
    let mesh = meshes.add(Triangle2d::new(
        Vec2::new(-triangle_size, triangle_size / 2.0),
        Vec2::new(triangle_size, triangle_size / 2.0),
        Vec2::new(0.0, -triangle_size / 2.0),
    ));
    let material = materials.add(Color::BLACK);

    commands.insert_resource(TriangleMesh { mesh, material });
}

#[derive(Component)]
struct Triangle {
    timer: Timer,
    up: bool,
}

fn animate_triangle(mut triangle: Query<(&mut Transform, &mut Triangle)>, time: Res<Time>) {
    for (mut transform, mut triangle) in &mut triangle {
        let delta = time.delta();

        if triangle.timer.tick(delta).just_finished() {
            let offset = if triangle.up {
                Vec3::new(0.0, -5.0, 0.0)
            } else {
                Vec3::new(0.0, 5.0, 0.0)
            };

            triangle.up = !triangle.up;
            transform.translation += offset;
        }
    }
}
