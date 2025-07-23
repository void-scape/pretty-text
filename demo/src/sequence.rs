use bevy::prelude::*;
use bevy_pretty_box::{PrettySequence, Textbox, TextboxName};
use bevy_pretty_text::effects::{Scramble, ScrambleLifetime, ScrambleSpeed};
use bevy_pretty_text::prelude::*;
use bevy_seedling::prelude::*;
use bevy_sequence::prelude::{FragmentExt, IntoFragment, spawn_root};

use crate::audio::formants::VoiceNode;

pub struct SequencePlugin;

impl Plugin for SequencePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, start_demo);
    }
}

fn start_demo(mut commands: Commands) {
    commands.spawn((
        PrettyStyle("highlight"),
        TextColor(Color::srgb_u8(13, 144, 104)),
    ));

    commands.spawn((
        PrettyStyle("scramble"),
        Scramble,
        ScrambleSpeed::Random(18f32..22f32),
        ScrambleLifetime::Always,
    ));

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
        set_observers(self.on_start(set_formant_freq(250f32)), || {
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
        set_observers(self.on_start(set_formant_freq(380f32)), || {
            children![
                Observer::new(crate::audio::voice::word_sfx),
                textbox_insert((
                    TypeWriter::new(4.5),
                    TypeWriterMode::Word,
                    TextboxName::new("Stranger"),
                )),
            ]
        })
    }

    fn creature(self) -> impl IntoFragment<PrettySequence> {
        set_observers(self.on_start(set_formant_freq(80f32)), || {
            children![
                Observer::new(crate::audio::voice::word_sfx),
                textbox_insert((
                    TypeWriter::new(4.5),
                    TypeWriterMode::Word,
                    TextboxName::new("Strange Creature"),
                )),
            ]
        })
    }
}

fn set_formant_freq(freq: f32) -> impl Fn(Single<&mut VoiceNode>) {
    move |mut voice: Single<&mut VoiceNode>| {
        voice.freq = freq;
        voice.pitch.set(freq);
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

fn demo() -> impl IntoFragment<PrettySequence> {
    (
        "It's a gentle night.".narrator(),
        "The tavern folk are `happy`[wave].[0.5] You are `happy`[wave].",
        1f32,
        "You see an old lady walking towards you. She is `shaking`[shake]<0.2>...<1>",
        1.5,
        "`Excuse me, good sir.[0.5] I have the most[0.5] \
            `regretfull`[!red, !scramble, wave] news...`[shake]"
            .shaker(),
        "I'm sorry?[0.5] <0.8>Did you just...[1] <1>`*gurgle*`[wobble]?".you(),
        pretty2d!(
            "But before she could continue, she fell with a loud \
                [0.3]{}<1.5>`plop`[!red, wave]!",
            fall,
        )
        .narrator(),
        "Heavens![1] What on earth is the matter?".you(),
        "She begins a rather strange metamorphosis<0.3>...".narrator(),
        1.5,
        "`Oh don't you worry about me, this will happen \
            from time to time`[!scramble]"
            .creature(),
        3f32,
        pretty2d!(
            "A `beetle`[shake]![1] Ha,[0.5] you have become a `beetle`[wave]![1] \
            From whence did you acquire this[1] {}`arcane`[!red]<0.5> magic?",
            bwah,
        )
        .you(),
        "You ask,[0.3] forgetfull of their condition.".narrator(),
        "`Don't mock me!`[!scramble]".creature(),
        "Alas,[0.5] if you will not speak with a `human`[!highlight] \
            tongue, [0.3]I must ask you to leave..."
            .you(),
        "`Very well`[!scramble]".creature(),
        1f32,
        pretty2d!(
            "The creature scitters away,{}[1] leaving the patrons none the wiser...",
            |mut commands: Commands, server: Res<AssetServer>| {
                commands.spawn(SamplePlayer::new(server.load("scitter.ogg")));
            }
        )
        .narrator()
        .on_end(exit),
    )
}

fn fall(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn(SamplePlayer::new(server.load("fall.wav")));
}

fn bwah(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn(SamplePlayer::new(server.load("bwah.wav")));
}

fn exit(mut writer: EventWriter<AppExit>) {
    writer.write(AppExit::Success);
}
