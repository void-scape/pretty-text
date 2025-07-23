use bevy::prelude::*;
use bevy_pretty_text::prelude::WordRevealed;
use bevy_seedling::{
    edge::Connect,
    node::RegisterNode,
    prelude::{AudioContext, MainBus},
    timeline::TimelineEvent,
};
use firewheel::clock::ClockSeconds;
use rand::Rng;

use super::formants::VoiceNode;
use crate::audio::formants::Vowel;

pub struct VoicesPlugin;

impl Plugin for VoicesPlugin {
    fn build(&self, app: &mut App) {
        app.register_node::<VoiceNode>()
            .add_systems(Startup, add_voice);
    }
}

fn add_voice(mut commands: Commands) {
    commands
        .spawn(VoiceNode::new())
        .connect_with(MainBus, &[(0, 0), (0, 1)]);
}

pub fn word_sfx(
    trigger: Trigger<WordRevealed>,
    mut voice: Single<&mut VoiceNode>,
    mut context: ResMut<AudioContext>,
) {
    let text = &trigger.text.trim_matches(['.', ',', '!', '?', '*']);

    let lookup_word = text.to_ascii_uppercase();
    let dictionary = cmumap::binary_map();
    let phonemes = dictionary.get(&lookup_word);

    use cmumap::phones::Phone;
    let get_vowel = |phoneme: cmumap::phones::Phone| {
        let vowel = match phoneme {
            Phone::AA(_) | Phone::AE(_) | Phone::AH(_) | Phone::AO(_) | Phone::AW(_) => {
                vec![Vowel::A]
            }
            Phone::AY(_) => vec![Vowel::A, Vowel::I],
            Phone::EH(_) | Phone::ER(_) | Phone::EY(_) => vec![Vowel::E],
            Phone::IH(_) | Phone::IY(_) => vec![Vowel::I],
            Phone::OW(_) => vec![Vowel::O, Vowel::U],
            Phone::OY(_) => vec![Vowel::O, Vowel::I],
            Phone::UH(_) | Phone::UW(_) => vec![Vowel::U],
            _ => return None,
        };

        Some(vowel)
    };

    let now = context.now();
    voice
        .gate
        .push(TimelineEvent::Deferred {
            value: 1.,
            time: now,
        })
        .unwrap();

    voice
        .gate
        .push(TimelineEvent::Deferred {
            value: 0.,
            time: now + ClockSeconds(0.15),
        })
        .unwrap();

    let freq = voice.freq;

    let mut rng = rand::rng();
    let variation = rng.random_range(0.70..1.30);

    if voice
        .pitch
        .push_curve(
            freq * variation,
            now,
            now + ClockSeconds(0.15),
            EaseFunction::Linear,
        )
        .is_err()
    {
        let value = voice.pitch.value_at(now);
        voice.pitch.set(value);
        voice
            .pitch
            .push_curve(
                freq * variation,
                now,
                now + ClockSeconds(0.15),
                EaseFunction::Linear,
            )
            .unwrap();
    }

    let vowel = phonemes.map(|p| {
        p.into_iter()
            .filter_map(|v| get_vowel(v.into_phone()))
            .flatten()
    });

    match vowel {
        Some(v) => {
            let time_increment = 0.075;
            for (i, vowel) in v.enumerate() {
                voice
                    .formant
                    .set_at(vowel, now + ClockSeconds(i as f64 * time_increment));
            }
        }
        None => {
            voice
                .formant
                .set_at(rng.random_range(0..5).try_into().unwrap(), now);
        }
    }
}
