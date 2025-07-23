//! Realtime voice synthesis inspired by `Celeste`
//!
//! <https://x.com/regameyk/status/1416483583053602816>
//!
//! Originally implemented here (with an older version of `bevy_seedling`)
//!
//! <https://github.com/void-scape/annual>

use super::deferred::Deferred;
use bevy::prelude::*;
use bevy_seedling::prelude::ChannelCount;
use bevy_seedling::timeline::Timeline;
use firewheel::channel_config::ChannelConfig;
use firewheel::clock::ClockSeconds;
use firewheel::diff::{Diff, Patch};
use firewheel::event::NodeEventList;
use firewheel::node::{AudioNodeProcessor, EmptyConfig, ProcBuffers, ProcInfo, ProcessStatus};
use fundsp::prelude::*;

type Adsr<F> = An<EnvelopeIn<f32, F, U1, f32>>;

fn adsr(
    attack: Shared,
    decay: Shared,
    sustain: Shared,
    release: Shared,
) -> Adsr<impl FnMut(f32, &Frame<f32, U1>) -> f32 + Clone> {
    let neg1 = -1.0;
    let zero = 0.0;

    let a = shared(zero);
    let b = shared(neg1);

    let attack_start = var(&a);
    let release_start = var(&b);
    envelope2(move |time, control| {
        if release_start.value() >= zero && control > zero {
            attack_start.set_value(time);
            release_start.set_value(neg1);
        } else if release_start.value() < zero && control <= zero {
            release_start.set_value(time);
        }
        let ads_value = ads(
            attack.value(),
            decay.value(),
            sustain.value(),
            time - attack_start.value(),
        );
        if release_start.value() < zero {
            ads_value
        } else {
            ads_value
                * clamp01(delerp(
                    release_start.value() + release.value(),
                    release_start.value(),
                    time,
                ))
        }
    })
}

fn ads<F: Float>(attack: F, decay: F, sustain: F, time: F) -> F {
    if time < attack {
        lerp(F::from_f64(0.0), F::from_f64(1.0), time / attack)
    } else {
        let decay_time = time - attack;
        if decay_time < decay {
            lerp(F::from_f64(1.0), sustain, decay_time / decay)
        } else {
            sustain
        }
    }
}

#[derive(Clone, Copy, Diff, Patch)]
pub enum Timbre {
    Bass,
    Tenor,
    Alto,
    Soprano,
}

impl Timbre {
    fn params(&self) -> &'static [[Formant; 5]; 5] {
        match self {
            Self::Bass => &BASS,
            Self::Tenor => &TENOR,
            Self::Alto => &ALTO,
            Self::Soprano => &SOPRANO,
        }
    }
}

#[derive(Clone, Copy, Diff, Patch, Debug)]
#[repr(u8)]
pub enum Vowel {
    A,
    E,
    I,
    O,
    U,
}

impl TryFrom<u8> for Vowel {
    type Error = ();

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        let vowel = match value {
            0 => Self::A,
            1 => Self::E,
            2 => Self::I,
            3 => Self::O,
            4 => Self::U,
            _ => return Err(()),
        };

        Ok(vowel)
    }
}

#[derive(Diff, Patch, Clone, Component)]
pub struct VoiceNode {
    pub freq: f32,
    pub pitch: Timeline<f32>,
    pub gate: Timeline<f32>,
    pub formant: Deferred<Vowel>,
    pub timbre: Timbre,
}

impl Default for VoiceNode {
    fn default() -> Self {
        Self::new()
    }
}

impl VoiceNode {
    pub fn new() -> VoiceNode {
        Self {
            freq: 320f32,
            pitch: Timeline::new(250f32),
            gate: Timeline::new(0f32),
            formant: Deferred::new(Vowel::A),
            timbre: Timbre::Tenor,
        }
    }
}

impl firewheel::node::AudioNode for VoiceNode {
    type Configuration = EmptyConfig;

    fn info(&self, _: &Self::Configuration) -> firewheel::node::AudioNodeInfo {
        firewheel::node::AudioNodeInfo::new()
            .debug_name("formant voice")
            .uses_events(true)
            .channel_config(ChannelConfig {
                num_inputs: ChannelCount::ZERO,
                num_outputs: ChannelCount::MONO,
            })
    }

    fn construct_processor(
        &self,
        _: &Self::Configuration,
        cx: firewheel::node::ConstructProcessorContext,
    ) -> impl AudioNodeProcessor {
        let gate = shared(0.);

        let attack = shared(0.015);
        let decay = shared(0.01);
        let sustain = shared(0.6);
        let release = shared(0.05);

        let frequency = shared(200.);

        let adsr = var(&gate)
            >> adsr(
                attack.clone(),
                decay.clone(),
                sustain.clone(),
                release.clone(),
            );

        let formant_params: Vec<_> = self.timbre.params()[0]
            .iter()
            .map(|f| {
                let (f, g, b) = f.into_params();

                (shared(f), shared(g), shared(b))
            })
            .collect();

        let formants = busi::<U5, _, _>(|i| {
            let (freq, gain, q) = &formant_params[i as usize];

            let smoothing = 0.05;
            let freq = var(freq) >> follow(smoothing);
            let q = var(q) >> follow(smoothing);
            let gain = var(gain) >> follow(smoothing);

            (pass() | freq | q) >> (bandpass::<f32>() * gain)
        });

        let voice = var(&frequency) >> saw();

        #[allow(clippy::precedence)]
        let mut processor = voice >> (formants * adsr) >> lowpass_hz(3000., 1.) * 1.0;

        processor.set_sample_rate(cx.stream_info.sample_rate.get() as f64);

        let updater = move |params: &VoiceNode| {
            gate.set(params.gate.get());
            frequency.set(params.pitch.get());

            let vowel = (params.formant.get() as i32).clamp(0, params.timbre.params().len() as i32);
            let vowel = &params.timbre.params()[vowel as usize];

            for (i, (freq, q, gain)) in formant_params.iter().enumerate() {
                let (new_freq, new_q, new_gain) = vowel[i].into_params();

                freq.set(new_freq);
                q.set(new_q);
                gain.set(new_gain);
            }
        };

        VoiceProcessor {
            params: self.clone(),
            graph: processor,
            updater,
            sample_rate_recip: cx.stream_info.sample_rate_recip,
        }
    }
}

struct VoiceProcessor<A, U> {
    params: VoiceNode,
    graph: A,
    updater: U,
    // updater: Box<dyn Fn(&VoiceNode) + Send + Sync>,
    sample_rate_recip: f64,
}

impl<A, U> AudioNodeProcessor for VoiceProcessor<A, U>
where
    A: AudioUnit + 'static,
    U: Fn(&VoiceNode) + Send + 'static,
{
    fn process(
        &mut self,
        ProcBuffers { outputs, .. }: ProcBuffers,
        info: &ProcInfo,
        mut events: NodeEventList,
    ) -> ProcessStatus {
        events.for_each_patch::<VoiceNode>(|p| self.params.apply(p));

        let time = info.clock_seconds.start;
        let increment = self.sample_rate_recip;

        for (frame, sample) in outputs[0].iter_mut().enumerate() {
            // update once every 16 samples
            if frame % 16 == 0 {
                let time = time + ClockSeconds(increment * frame as f64);

                self.params.pitch.tick(time);
                self.params.gate.tick(time);
                self.params.formant.tick(time);
                (self.updater)(&self.params);
            }

            *sample = self.graph.get_mono();
        }

        ProcessStatus::outputs_not_silent()
    }

    fn new_stream(&mut self, stream_info: &firewheel::StreamInfo) {
        self.sample_rate_recip = stream_info.sample_rate_recip;
        self.graph
            .set_sample_rate(stream_info.sample_rate.get() as f64);
    }
}

fn db(db: f32) -> f32 {
    10f32.powf(db / 20.0)
}

#[derive(Clone, Copy)]
struct Formant {
    frequency: f32,
    amplitude: f32,
    bandwidth: f32,
}

impl Formant {
    fn into_params(self) -> (f32, f32, f32) {
        (
            self.frequency,
            db(self.amplitude),
            self.frequency / self.bandwidth,
        )
    }
}

const BASS: [[Formant; 5]; 5] = [
    // "a"
    [
        Formant {
            frequency: 600.0,
            amplitude: 0.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 1040.0,
            amplitude: -7.0,
            bandwidth: 70.0,
        },
        Formant {
            frequency: 2250.0,
            amplitude: -9.0,
            bandwidth: 110.0,
        },
        Formant {
            frequency: 2450.0,
            amplitude: -9.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 2750.0,
            amplitude: -20.0,
            bandwidth: 130.0,
        },
    ],
    // "e"
    [
        Formant {
            frequency: 400.0,
            amplitude: 0.0,
            bandwidth: 40.0,
        },
        Formant {
            frequency: 1620.0,
            amplitude: -12.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2400.0,
            amplitude: -9.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2800.0,
            amplitude: -12.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3100.0,
            amplitude: -18.0,
            bandwidth: 120.0,
        },
    ],
    // "i"
    [
        Formant {
            frequency: 250.0,
            amplitude: 0.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 1750.0,
            amplitude: -30.0,
            bandwidth: 90.0,
        },
        Formant {
            frequency: 2600.0,
            amplitude: -16.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 3050.0,
            amplitude: -22.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3340.0,
            amplitude: -28.0,
            bandwidth: 120.0,
        },
    ],
    // "o"
    [
        Formant {
            frequency: 400.0,
            amplitude: 0.0,
            bandwidth: 40.0,
        },
        Formant {
            frequency: 750.0,
            amplitude: -11.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2400.0,
            amplitude: -21.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2600.0,
            amplitude: -20.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 2900.0,
            amplitude: -40.0,
            bandwidth: 120.0,
        },
    ],
    // "u"
    [
        Formant {
            frequency: 350.0,
            amplitude: 0.0,
            bandwidth: 40.0,
        },
        Formant {
            frequency: 600.0,
            amplitude: -20.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2400.0,
            amplitude: -32.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2675.0,
            amplitude: -28.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 2950.0,
            amplitude: -36.0,
            bandwidth: 120.0,
        },
    ],
];

const TENOR: [[Formant; 5]; 5] = [
    [
        Formant {
            frequency: 650.0,
            amplitude: 0.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 1080.0,
            amplitude: -6.0,
            bandwidth: 90.0,
        },
        Formant {
            frequency: 2650.0,
            amplitude: -7.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 2900.0,
            amplitude: -8.0,
            bandwidth: 130.0,
        },
        Formant {
            frequency: 3250.0,
            amplitude: -22.0,
            bandwidth: 140.0,
        },
    ],
    [
        Formant {
            frequency: 400.0,
            amplitude: 0.0,
            bandwidth: 70.0,
        },
        Formant {
            frequency: 1700.0,
            amplitude: -14.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2600.0,
            amplitude: -12.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 3200.0,
            amplitude: -14.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3580.0,
            amplitude: -20.0,
            bandwidth: 120.0,
        },
    ],
    [
        Formant {
            frequency: 290.0,
            amplitude: 0.0,
            bandwidth: 40.0,
        },
        Formant {
            frequency: 1870.0,
            amplitude: -15.0,
            bandwidth: 90.0,
        },
        Formant {
            frequency: 2800.0,
            amplitude: -18.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 3250.0,
            amplitude: -20.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3540.0,
            amplitude: -30.0,
            bandwidth: 120.0,
        },
    ],
    [
        Formant {
            frequency: 400.0,
            amplitude: 0.0,
            bandwidth: 40.0,
        },
        Formant {
            frequency: 800.0,
            amplitude: -10.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2600.0,
            amplitude: -12.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2800.0,
            amplitude: -12.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3000.0,
            amplitude: -26.0,
            bandwidth: 120.0,
        },
    ],
    [
        Formant {
            frequency: 350.0,
            amplitude: 0.0,
            bandwidth: 40.0,
        },
        Formant {
            frequency: 600.0,
            amplitude: -20.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 2700.0,
            amplitude: -17.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2900.0,
            amplitude: -14.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3300.0,
            amplitude: -26.0,
            bandwidth: 120.0,
        },
    ],
];

const SOPRANO: [[Formant; 5]; 5] = [
    [
        Formant {
            frequency: 800.0,
            amplitude: 0.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 1150.0,
            amplitude: -6.0,
            bandwidth: 90.0,
        },
        Formant {
            frequency: 2900.0,
            amplitude: -32.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3900.0,
            amplitude: -20.0,
            bandwidth: 130.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -50.0,
            bandwidth: 140.0,
        },
    ],
    [
        Formant {
            frequency: 350.0,
            amplitude: 0.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 2000.0,
            amplitude: -20.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2800.0,
            amplitude: -15.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3600.0,
            amplitude: -40.0,
            bandwidth: 150.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -56.0,
            bandwidth: 200.0,
        },
    ],
    [
        Formant {
            frequency: 270.0,
            amplitude: 0.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 2140.0,
            amplitude: -12.0,
            bandwidth: 90.0,
        },
        Formant {
            frequency: 2950.0,
            amplitude: -26.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 3900.0,
            amplitude: -26.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -44.0,
            bandwidth: 120.0,
        },
    ],
    [
        Formant {
            frequency: 450.0,
            amplitude: 0.0,
            bandwidth: 70.0,
        },
        Formant {
            frequency: 800.0,
            amplitude: -11.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2830.0,
            amplitude: -22.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 3800.0,
            amplitude: -22.0,
            bandwidth: 130.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -50.0,
            bandwidth: 135.0,
        },
    ],
    [
        Formant {
            frequency: 325.0,
            amplitude: 0.0,
            bandwidth: 50.0,
        },
        Formant {
            frequency: 700.0,
            amplitude: -16.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 2700.0,
            amplitude: -35.0,
            bandwidth: 170.0,
        },
        Formant {
            frequency: 3800.0,
            amplitude: -40.0,
            bandwidth: 180.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -60.0,
            bandwidth: 200.0,
        },
    ],
];

const ALTO: [[Formant; 5]; 5] = [
    // "a"
    [
        Formant {
            frequency: 800.0,
            amplitude: 0.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 1150.0,
            amplitude: -4.0,
            bandwidth: 90.0,
        },
        Formant {
            frequency: 2800.0,
            amplitude: -20.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3500.0,
            amplitude: -36.0,
            bandwidth: 130.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -60.0,
            bandwidth: 140.0,
        },
    ],
    // "e"
    [
        Formant {
            frequency: 400.0,
            amplitude: 0.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 1600.0,
            amplitude: -24.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2700.0,
            amplitude: -30.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3300.0,
            amplitude: -35.0,
            bandwidth: 150.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -60.0,
            bandwidth: 200.0,
        },
    ],
    // "i"
    [
        Formant {
            frequency: 350.0,
            amplitude: 0.0,
            bandwidth: 50.0,
        },
        Formant {
            frequency: 1700.0,
            amplitude: -20.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 2700.0,
            amplitude: -30.0,
            bandwidth: 120.0,
        },
        Formant {
            frequency: 3700.0,
            amplitude: -36.0,
            bandwidth: 150.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -60.0,
            bandwidth: 200.0,
        },
    ],
    // "o"
    [
        Formant {
            frequency: 450.0,
            amplitude: 0.0,
            bandwidth: 70.0,
        },
        Formant {
            frequency: 800.0,
            amplitude: -9.0,
            bandwidth: 80.0,
        },
        Formant {
            frequency: 2830.0,
            amplitude: -16.0,
            bandwidth: 100.0,
        },
        Formant {
            frequency: 3500.0,
            amplitude: -28.0,
            bandwidth: 130.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -55.0,
            bandwidth: 135.0,
        },
    ],
    // "u"
    [
        Formant {
            frequency: 325.0,
            amplitude: 0.0,
            bandwidth: 50.0,
        },
        Formant {
            frequency: 700.0,
            amplitude: -12.0,
            bandwidth: 60.0,
        },
        Formant {
            frequency: 2530.0,
            amplitude: -30.0,
            bandwidth: 170.0,
        },
        Formant {
            frequency: 3500.0,
            amplitude: -40.0,
            bandwidth: 180.0,
        },
        Formant {
            frequency: 4950.0,
            amplitude: -64.0,
            bandwidth: 200.0,
        },
    ],
];
