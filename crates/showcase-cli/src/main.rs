use bevy::prelude::*;
use bevy::window::WindowResolution;
use bevy_pretty_text::effects::PrettyEffectSet;
use bevy_pretty_text::prelude::*;
use clap::Parser;

/// Tool for generating bevy_pretty_text showcase GIFs.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The directory that the GIFs are written to.
    #[arg(short, long, default_value_t = String::from("./out"))]
    output_path: String,
}

fn main() {
    let Args { output_path } = Args::parse();

    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    resolution: WindowResolution::new(490f32, 274f32),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            PrettyTextPlugin,
        ))
        .insert_resource(ClearColor(Color::srgb(0.166, 0.156, 0.186)))
        .add_systems(Startup, |mut commands: Commands| {
            commands.spawn(Camera2d);
        })
        .add_systems(Update, spawn_text.before(PrettyEffectSet))
        .run();
}

#[derive(Clone, Copy)]
struct EffectScene(f32, f32, &'static str, Option<f32>);

fn scenes() -> &'static [EffectScene] {
    &[
        //
        // appearance
        EffectScene(3f32, 1f32, "`SPREAD`[spread]", Some(10f32)),
        EffectScene(3f32, 0.8, "`SCRAMBLE`[scramble]", Some(10f32)),
        //
        // behaviors
        EffectScene(3f32, 1f32, "`WAVING`[wave]", None),
        EffectScene(3f32, 1f32, "`WOBBLE`[wobble]", None),
        EffectScene(3f32, 1f32, "`SHAKE`[shake]", None),
        EffectScene(3f32, 0.7f32, "`BREATHING`[breathe]", None),
        EffectScene(3f32, 1f32, "`PIVOT`[pivot]", None),
        EffectScene(3f32, 0.8f32, "`SPINNING`[spin]", None),
        EffectScene(3f32, 0.9, "`RAINBOW`[rainbow]", None),
        EffectScene(3f32, 1f32, "`GLITCH`[glitch]", None),
    ]
}

#[derive(Component)]
struct Root;

fn spawn_text(
    mut commands: Commands,
    time: Res<Time>,
    mut writer: EventWriter<AppExit>,
    mut index: Local<usize>,
    mut timer: Local<Timer>,
    query: Query<Entity, With<Root>>,
) {
    timer.tick(time.delta());
    if timer.just_finished() {
        let scenes = scenes();
        if *index >= scenes.len() {
            writer.write(AppExit::Success);
            return;
        }
        let scene = scenes[*index].clone();
        *timer = Timer::from_seconds(scene.0, TimerMode::Once);
        for entity in query.iter() {
            commands.entity(entity).despawn();
        }
    } else {
        return;
    }

    let scenes = scenes();
    if *index >= scenes.len() {
        writer.write(AppExit::Success);
        return;
    }
    let scene = scenes[*index].clone();
    *index += 1;

    let text = scene.2;
    let calculated_size = 274f32 / 2.4f32 * scene.1;
    let font = TextFont {
        font_size: calculated_size,
        ..Default::default()
    };
    let node = Node {
        width: Val::Percent(100f32),
        height: Val::Percent(100f32),
        justify_content: JustifyContent::Center,
        align_items: AlignItems::Center,
        ..Default::default()
    };
    let layout = TextLayout {
        justify: JustifyText::Center,
        linebreak: LineBreak::WordBoundary,
    };

    if let Some(speed) = scene.3 {
        // commands.spawn((
        //     Root,
        //     layout,
        //     PrettyParser2d::bundle(text).unwrap(),
        //     Typewriter::new(speed),
        //     font,
        // ));
        commands.spawn((
            Root,
            node,
            children![(
                layout,
                PrettyParser::bundle(text).unwrap(),
                Typewriter::new(speed),
                font
            )],
        ));
    } else {
        // commands.spawn((Root, layout, PrettyParser2d::bundle(text).unwrap(), font));
        commands.spawn((
            Root,
            node,
            children![(layout, PrettyParser::bundle(text).unwrap(), font)],
        ));
    }
}
