use bevy::prelude::*;
use bevy_pretty_text::type_writer::sound::WordEvent;
use bevy_seedling::{AudioContext, ConnectNode, MainBus, RegisterParamsNode};
use firewheel::clock::ClockSeconds;
use rand::Rng;

use super::formants::VoiceNode;

pub struct VoicesPlugin;

impl Plugin for VoicesPlugin {
    fn build(&self, app: &mut App) {
        app.register_params_node::<VoiceNode>()
            .add_systems(Startup, add_voice)
            .add_systems(Update, play_voice);
    }
}

fn add_voice(mut commands: Commands) {
    commands
        .spawn(VoiceNode::new())
        .connect_with(MainBus, &[(0, 0), (0, 1)]);
}

fn play_voice(
    mut reader: EventReader<WordEvent>,
    mut voice: Single<&mut VoiceNode>,
    mut context: ResMut<AudioContext>,
) {
    for _ in reader.read() {
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

        let freq = 320.;

        let mut rng = rand::thread_rng();

        let variation = rng.gen_range(0.70..1.30);

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

        voice.formant.push(DeferredEvent::Deferred {
            value: rng.gen_range(0..5),
            time: now,
        });
    }
}
