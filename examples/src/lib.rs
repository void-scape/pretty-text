use bevy::prelude::*;
use bevy::render::render_resource::AsBindGroup;
use bevy::shader::ShaderRef;
use bevy::sprite_render::{Material2d, Material2dPlugin};
use bevy::window::WindowResolution;

pub struct BalatroPlugin;

impl Plugin for BalatroPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            DefaultPlugins
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        resolution: WindowResolution::new(1280, 720),
                        ..Default::default()
                    }),
                    ..Default::default()
                })
                .set(ImagePlugin::default_nearest()),
            Material2dPlugin::<BalatroMaterial>::default(),
        ))
        .add_systems(
            Startup,
            |mut commands: Commands, server: Res<AssetServer>| {
                commands.spawn((
                    Mesh2d(server.add(Rectangle::new(1280f32, 720f32).mesh().build())),
                    MeshMaterial2d(server.add(BalatroMaterial {})),
                ));
            },
        );
    }
}

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
struct BalatroMaterial {}

impl Material2d for BalatroMaterial {
    fn fragment_shader() -> ShaderRef {
        "balatro.wgsl".into()
    }
}
