use bevy::prelude::*;
use bevy_pretty_text::prelude::*;
use prettytext_examples::BalatroPlugin;

fn main() {
    App::new()
        .add_plugins((BalatroPlugin, PrettyTextPlugin))
        .add_systems(Startup, spawn_text)
        .run();
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
        EffectScene(0.9, "`FADE IN`[fade_in][2]", Some(6f32)),
        EffectScene(0.8, "`SCRAMBLE`[scramble][2]", Some(6f32)),
        EffectScene(1f32, "`SPREAD`[spread][2]", Some(6f32)),
    ]
}

fn spawn_text(mut commands: Commands) {
    commands.spawn(Camera2d);
    let scenes = scenes();
    let len = scenes.len();

    let layout = TextLayout {
        justify: JustifyText::Center,
        linebreak: LineBreak::WordBoundary,
    };

    let width = (len as f32).sqrt().ceil() as usize;
    let height = width;

    let scale = 2.0;

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
            let calculated_size = 58.0 / 2.4 * scene.0 * scale;
            let font = TextFont {
                font_size: calculated_size,
                ..Default::default()
            };

            if let Some(speed) = scene.2 {
                commands
                    .spawn((
                        layout,
                        PrettyParser2d::bundle(text).unwrap(),
                        Typewriter::new(speed),
                        font,
                        Transform::from_xyz(
                            (w as f32 * w_size - w_offset) * scale,
                            (h as f32 * h_size - h_offset) * scale,
                            0.,
                        ),
                    ))
                    .observe(
                        move |trigger: Trigger<TypewriterFinished>, mut commands: Commands| {
                            // Restart by inserting another type writer.
                            commands
                                .entity(trigger.target())
                                .insert(Typewriter::new(speed));
                        },
                    );
            } else {
                commands.spawn((
                    layout,
                    PrettyParser2d::bundle(text).unwrap(),
                    font.clone(),
                    Transform::from_xyz(
                        (w as f32 * w_size - w_offset) * scale,
                        (h as f32 * h_size - h_offset) * scale,
                        0.,
                    ),
                ));
            }
        }
    }
}
