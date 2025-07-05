use std::time::Duration;

use bevy::prelude::*;
use bevy::sprite::Anchor;
use bevy::text::TextBounds;
use bevy::window::WindowResolution;
use bevy_pretty_text::prelude::TypeWriter;
use bevy_pretty_text::style::StyleAppExt;
use bevy_sequence::prelude::{FragmentExt, IntoFragment, spawn_root};

use bevy_pretty_box::{
    AudioSequence, CharacterFragment, Textbox, TextboxContainer, TextboxContinue, TextboxName,
    despawn_textbox,
};

fn main() {
    App::default()
        .add_plugins((
            DefaultPlugins
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        resolution: WindowResolution::new(768.0 * 1.5, 560.0 * 1.5),
                        ..Default::default()
                    }),
                    ..Default::default()
                })
                .set(ImagePlugin::default_nearest()),
            bevy_pretty_box::PrettyBoxPlugin,
            bevy_egui::EguiPlugin {
                enable_multipass_for_primary_context: true,
            },
            bevy_inspector_egui::quick::WorldInspectorPlugin::new(),
        ))
        .insert_resource(ClearColor(Color::BLACK))
        .add_systems(Startup, (startup, generate_triangle))
        .add_systems(Update, animate_triangle)
        .register_pretty_style("highlight", |_| Color::srgb_u8(13, 144, 104))
        .add_observer(textbox_container)
        .add_observer(textbox)
        .add_observer(textbox_name)
        .add_observer(textbox_continue)
        .run();
}

fn startup(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn(Camera2d);
    commands.spawn((
        Name::new("Desk"),
        Sprite::from_image(server.load("paper-ui/desk.png")),
        Transform::from_xyz(0., 0., -1.).with_scale(Vec3::splat(1.5)),
    ));
    spawn_root(demo().always().once(), &mut commands);
}

fn textbox_container(
    trigger: Trigger<OnAdd, TextboxContainer>,
    mut commands: Commands,
    server: Res<AssetServer>,
) {
    commands
        .entity(trigger.target())
        .insert(Transform::from_xyz(0., 25., 0.))
        .with_child((
            Name::new("Textbox Sprite"),
            Sprite::from_image(server.load("paper-ui/paper.png")),
            Transform::from_scale(Vec3::splat(1.5)),
        ));
}

fn textbox(trigger: Trigger<OnAdd, Textbox>, mut commands: Commands, server: Res<AssetServer>) {
    commands.entity(trigger.target()).insert((
        TypeWriter::new(35.),
        TextBounds::new_horizontal(378. * 2.),
        TextFont {
            font_size: 48.0,
            font: server.load("canterbury/Canterbury.ttf"),
            ..Default::default()
        },
        TextColor(Color::BLACK),
        Anchor::TopLeft,
        Transform::from_xyz(-378., 0., 0.),
    ));
}

fn textbox_name(trigger: Trigger<OnAdd, TextboxName>, mut commands: Commands) {
    commands
        .entity(trigger.target())
        .observe(
            |trigger: Trigger<OnAdd, Text2d>, mut commands: Commands, server: Res<AssetServer>| {
                commands
                    .entity(trigger.target())
                    .insert((
                        Transform::from_xyz(-240., 138., 1.),
                        TextFont {
                            font_size: 48.0,
                            font: server.load("canterbury/Canterbury.ttf"),
                            ..Default::default()
                        },
                        TextColor(Color::BLACK),
                    ))
                    .with_child((
                        Sprite::from_image(server.load("paper-ui/speaker-header.png")),
                        Transform::from_xyz(0.0, -9.0, 0.0).with_scale(Vec3::splat(1.5)),
                    ));
            },
        )
        .observe(
            |trigger: Trigger<OnRemove, Text2d>,
             mut commands: Commands,
             children: Query<&Children>| {
                if let Ok(children) = children.get(trigger.target()) {
                    for child in children.iter() {
                        // this can fail during the clean up process
                        if let Ok(mut entity) = commands.get_entity(child) {
                            entity.despawn();
                        }
                    }
                }
            },
        );
}

fn textbox_continue(
    trigger: Trigger<OnAdd, TextboxContinue>,
    mut commands: Commands,
    triangle: Res<TriangleMesh>,
) {
    commands.entity(trigger.target()).insert((
        Transform::from_translation(Default::default()),
        Triangle {
            timer: Timer::new(Duration::from_secs_f32(0.5), TimerMode::Repeating),
            up: false,
        },
        triangle.mesh(),
    ));
}

fn demo() -> impl IntoFragment<AudioSequence> {
    (
        intro().on_end(despawn_textbox),
        creek().on_end(despawn_textbox),
    )
}

fn intro() -> impl IntoFragment<AudioSequence> {
    (
        "It's a gentle night.".narrator(),
        "The moon peeks behind the clouds.",
        "The wind blows through the tall trees.",
        1.5,
        "You see someone walking towards you.",
        "Oh no<0.2>... [1]<1>he wants to <0.5>`talk to you`[shake(1, 3)]...",
        1.5,
        "Hey there!".stranger(),
        "Lovely night, isn't it~",
        "<0.2>...[1]<1> he seems too happy...".narrator(),
        "Mind if I join you?".stranger(),
        1.0,
        "You can't think of an excuse, [0.5]so unfortunately you have to accept.",
        3.0,
    )
}

fn creek() -> impl IntoFragment<AudioSequence> {
    (
        "My name's `Aster|highlight`.[1] Pleased to meet you!".stranger(),
        2.5,
        "Aster runs his hand absent-mindedly though some chimes.".narrator(),
        "(Who put chimes out here?)",
        3.0,
        "Don't you love the sound of pine trees in the wind?".aster(),
        "It almost sounds like<0.2>...[0.5]<1> I don't know,[0.5] a big[0.33] river or something.",
    )
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
    let material = materials.add(Color::WHITE);

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
