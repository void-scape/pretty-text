use bevy::prelude::*;
use bevy_pretty_box::{Character, CharacterFragment, PrettySequence, despawn_textbox};
use bevy_pretty_text::style::StyleAppExt;
use bevy_sequence::prelude::{FragmentExt, IntoFragment, spawn_root};

pub struct SequencePlugin;

impl Plugin for SequencePlugin {
    fn build(&self, app: &mut App) {
        app.register_pretty_style("highlight", |_| Color::srgb_u8(13, 144, 104))
            .insert_resource(Character {
                name: None,
                text_sound: "talk-low.wav",
            })
            .add_systems(Startup, start_demo);
    }
}

fn start_demo(mut commands: Commands) {
    spawn_root(demo().always().once(), &mut commands);
}

pub fn demo() -> impl IntoFragment<PrettySequence> {
    (
        intro().on_end(despawn_textbox),
        creek().on_end(despawn_textbox),
    )
}

fn intro() -> impl IntoFragment<PrettySequence> {
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

fn creek() -> impl IntoFragment<PrettySequence> {
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
