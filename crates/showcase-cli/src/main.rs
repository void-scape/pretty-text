use std::fs::File;
use std::time::Duration;

use bevy::app::{RunMode, ScheduleRunnerPlugin};
use bevy::prelude::*;
use bevy::render::RenderPlugin;
use bevy::time::TimeUpdateStrategy;
use bevy::window::WindowResolution;
use bevy::winit::WinitPlugin;
use bevy_capture::encoder::gif::{self, GifEncoder};
use bevy_capture::{CameraTargetHeadless, Capture, CaptureBundle};
use bevy_pretty_text::prelude::*;
use clap::Parser;
use indicatif::ProgressBar;

const FRAMES: u32 = 24;

/// Tool for generating bevy_pretty_text showcase GIFs.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Capture GIFs for each effect. With `capture` enabled, the app will run headlessly.
    #[arg(short, long, default_value_t = false)]
    capture: bool,
}

const WIDTH: f32 = 500.0;
const HEIGHT: f32 = 300.0;

fn main() {
    let Args { capture } = Args::parse();

    let mut app = App::new();

    if capture {
        app.add_plugins((
            DefaultPlugins
                .build()
                // Disable the WinitPlugin to prevent the creation of a window
                .disable::<WinitPlugin>()
                // Make sure pipelines are ready before rendering
                .set(RenderPlugin {
                    synchronous_pipeline_compilation: true,
                    ..default()
                }),
            // Add the ScheduleRunnerPlugin to run the app in loop mode
            ScheduleRunnerPlugin {
                run_mode: RunMode::Loop { wait: None },
            },
            // Add the CapturePlugin
            bevy_capture::CapturePlugin,
        ));

        app.insert_resource(TimeUpdateStrategy::ManualDuration(Duration::from_secs_f64(
            1.0 / 10.0,
        )));
    } else {
        app.add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                resolution: WindowResolution::new(WIDTH, HEIGHT),
                ..Default::default()
            }),
            ..Default::default()
        }));
    }

    app.add_plugins((
        PrettyTextPlugin,
        // bevy_inspector_egui::bevy_egui::EguiPlugin::default(),
        // bevy_inspector_egui::quick::WorldInspectorPlugin::default(),
    ))
    .insert_resource(ClearColor(Color::srgb(0.166, 0.156, 0.186)))
    .add_systems(
        Startup,
        move |mut commands: Commands, mut images: ResMut<Assets<Image>>| {
            if capture {
                commands.spawn((
                    Camera2d,
                    Camera::default().target_headless(WIDTH as u32, HEIGHT as u32, &mut images),
                    CaptureBundle::default(),
                ));
            } else {
                commands.spawn(Camera2d);
            }
        },
    )
    .add_systems(
        Update,
        (
            capture_text.run_if(after_ticks(3)),
            spawn_text.run_if(run_once),
        ),
    )
    .add_systems(Update, frame.run_if(move || capture));

    app.run();
}

fn after_ticks(ticks: u32) -> impl Fn(Local<u32>) -> bool {
    move |mut t: Local<u32>| {
        *t += 1;
        *t > ticks
    }
}

#[derive(Clone, Copy)]
struct EffectScene(f32, &'static str, Option<f32>);

fn scenes() -> &'static [EffectScene] {
    &[
        // behaviors
        EffectScene(1f32, "`BOUNCE`[bounce]", None),
        EffectScene(0.7f32, "`BREATHING`[breathe]", None),
        EffectScene(1f32, "`FADE`[fade]", None),
        EffectScene(1f32, "`GLITCH`[glitch]", None),
        EffectScene(1f32, "`PIVOT`[pivot]", None),
        EffectScene(0.9, "`RAINBOW`[rainbow]", None),
        EffectScene(1f32, "`SHAKE`[shake]", None),
        EffectScene(0.8f32, "`SPINNING`[spin]", None),
        EffectScene(1f32, "`WOBBLE`[wobble]", None),
        EffectScene(1f32, "`WAVING`[wave]", None),
        // appearance
        EffectScene(0.9, "`FADE IN`[fade_in]", Some(6f32)),
        EffectScene(0.8, "`SCRAMBLE`[scramble]", Some(6f32)),
        EffectScene(1f32, "`SPREAD`[spread]", Some(6f32)),
    ]
}

#[derive(Component)]
struct Root;

fn spawn_text(mut commands: Commands) {
    let scenes = scenes();
    let len = scenes.len();

    let layout = TextLayout {
        justify: JustifyText::Center,
        linebreak: LineBreak::WordBoundary,
    };

    let width = (len as f32).sqrt().ceil() as usize;
    let height = width;

    let w_size = 120.0;
    let h_size = -50.0;

    let w_offset = (len as f32).sqrt().floor() * w_size / 2.0;
    let h_offset = (len as f32).sqrt().floor() * h_size / 2.0;

    let mut i = 0;
    for h in 0..height {
        for w in 0..width {
            if i >= len {
                break;
            }

            let scene = scenes[i];
            i += 1;
            let text = scene.1;
            let calculated_size = 58.0 / 2.4 * scene.0;
            let font = TextFont {
                font_size: calculated_size,
                ..Default::default()
            };

            if let Some(speed) = scene.2 {
                commands.spawn((
                    Root,
                    layout,
                    PrettyParser2d::bundle(text).unwrap(),
                    Typewriter::new(speed),
                    font,
                    Transform::from_xyz(
                        w as f32 * w_size - w_offset,
                        h as f32 * h_size - h_offset,
                        0.,
                    ),
                ));
            } else {
                commands.spawn((
                    Root,
                    layout,
                    PrettyParser2d::bundle(text).unwrap(),
                    font.clone(),
                    Transform::from_xyz(
                        w as f32 * w_size - w_offset,
                        h as f32 * h_size - h_offset,
                        0.,
                    ),
                ));
            }
        }
    }
}

fn capture_text(mut capture: Single<&mut Capture>) {
    if !capture.is_capturing() {
        capture.start(
            GifEncoder::new(File::create("showcase.gif").unwrap())
                .with_repeat(gif::Repeat::Infinite),
        );
    }
}

fn frame(
    mut writer: EventWriter<AppExit>,
    mut frame: Local<u32>,
    mut bar: Local<Option<ProgressBar>>,
) {
    let bar = bar.get_or_insert_with(|| ProgressBar::new(FRAMES as u64));
    bar.inc(1);

    *frame += 1;
    if *frame >= FRAMES {
        bar.finish();
        writer.write(AppExit::Success);
    }
}
