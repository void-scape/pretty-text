use bevy::prelude::*;
use bevy_pretty_box::{PrettySequence, Textbox, TextboxName, despawn_textbox};
use bevy_pretty_text::prelude::{GlyphRevealed, TypeWriter, TypeWriterMode};
use bevy_pretty_text::pretty;
use bevy_pretty_text::style::StyleAppExt;
use bevy_seedling::prelude::*;
use bevy_sequence::prelude::{FragmentExt, IntoFragment, spawn_root};

pub struct SequencePlugin;

impl Plugin for SequencePlugin {
    fn build(&self, app: &mut App) {
        app.register_pretty_style("highlight", |_| Color::srgb_u8(13, 144, 104))
            .add_systems(Startup, start_demo);
    }
}

fn start_demo(mut commands: Commands) {
    spawn_root(demo().always().once(), &mut commands);
}

impl<T> CharacterFragment for T where T: IntoFragment<PrettySequence> {}
pub trait CharacterFragment
where
    Self: Sized + IntoFragment<PrettySequence>,
{
    fn narrator(self) -> impl IntoFragment<PrettySequence> {
        set_observers(self, || {
            children![
                glyph_sfx("talk-low.wav", 1.0),
                textbox_insert(TypeWriter::new(35.0)),
            ]
        })
    }

    fn you(self) -> impl IntoFragment<PrettySequence> {
        set_observers(self, || {
            children![
                Observer::new(crate::audio::voice::word_sfx),
                textbox_insert((
                    TypeWriter::new(5.5),
                    TypeWriterMode::Word,
                    TextboxName::new("You"),
                )),
            ]
        })
    }

    fn shaker(self) -> impl IntoFragment<PrettySequence> {
        set_observers(self, || {
            children![
                Observer::new(crate::audio::voice::word_sfx),
                textbox_insert((
                    TypeWriter::new(5.5),
                    TypeWriterMode::Word,
                    TextboxName::new("Stranger"),
                )),
            ]
        })
    }
}

fn textbox_insert(bundle: impl Bundle + Clone) -> impl Bundle {
    Observer::new(
        move |trigger: Trigger<OnInsert, Textbox>, mut commands: Commands| {
            commands.entity(trigger.target()).insert(bundle.clone());
        },
    )
}

fn glyph_sfx(sfx: &'static str, volume: f32) -> impl Bundle {
    Observer::new(
        move |trigger: Trigger<GlyphRevealed>, mut commands: Commands, server: Res<AssetServer>| {
            if trigger.text != " " {
                commands
                    .spawn(SamplePlayer::new(server.load(sfx)).with_volume(Volume::Linear(volume)));
            }
        },
    )
}

fn set_observers<B: Bundle>(
    frag: impl IntoFragment<PrettySequence>,
    observers: impl Fn() -> B + Send + Sync + 'static,
) -> impl IntoFragment<PrettySequence> {
    //
    #[derive(Component)]
    struct Observers;

    frag.on_start(
        |mut commands: Commands, last_character: Option<Single<Entity, With<Observers>>>| {
            if let Some(last_character) = last_character {
                commands.entity(*last_character).despawn();
            }
        },
    )
    .on_start(move |mut commands: Commands| {
        commands.spawn((observers(), Observers));
    })
}

pub fn demo() -> impl IntoFragment<PrettySequence> {
    (intro().on_end(despawn_textbox),)
}

fn intro() -> impl IntoFragment<PrettySequence> {
    (
        "It's a gentle night.".narrator(),
        "The tavern folk are `happy`[wave(0.5, 10)].[0.5] You are `happy`[wave(0.5, 10)].",
        1.0,
        "You see someone walking towards you. They are `shaking`[shake(1, 3)]<0.2>...<1>",
        1.5,
        "`Excuse me, good sir.[0.5] I have the most[0.5] `regretfull|red`[wave(0.5, 10)] news...`[shake(0.5, 1.5)]"
            .shaker(),
        "Go on.".you(),
        pretty!(
            "But before she could continue, she fell with a loud {}<1.5>[1]`plop`[wobble(1, 2)]!<1>",
            |mut commands: Commands, server: Res<AssetServer>| {
                commands.spawn(SamplePlayer::new(server.load("fall.ogg")));
            }
        )
        .narrator()
    )
}
