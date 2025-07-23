use bevy::prelude::*;
use bevy_seedling::prelude::*;

mod deferred;
pub mod formants;
pub mod voice;

pub struct AudioPlugin;

impl Plugin for AudioPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(voice::VoicesPlugin)
            .add_systems(Startup, tavern_ambience);
    }
}

fn tavern_ambience(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn(SamplePlayer::new(server.load("harp.ogg")).looping());
    commands.spawn(
        SamplePlayer::new(server.load("tavern.ogg"))
            .looping()
            .with_volume(Volume::Linear(0.5)),
    );
}
