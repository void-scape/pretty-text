<img src="https://raw.githubusercontent.com/void-scape/bevy_pretty_text/v0.3.0/assets/title-card.gif" width="100%">

[![crates.io](https://img.shields.io/crates/v/bevy_pretty_text)](https://crates.io/crates/bevy_pretty_text)
[![docs.rs](https://docs.rs/bevy_pretty_text/badge.svg)](https://docs.rs/bevy_pretty_text)

**Pretty Text** is a text effects library for [Bevy](https://bevyengine.org/).

## Demos

`cargo run --bin showcase`<br>

<img src="https://raw.githubusercontent.com/void-scape/bevy_pretty_text/v0.3.0/assets/showcase.gif" width="480" alt="All text effects">

---

`cargo run --bin typewriter`<br>

<img src="https://raw.githubusercontent.com/void-scape/bevy_pretty_text/v0.3.0/assets/typewriter.gif" width="480" alt="A typewriter demonstration">

---

> [!NOTE]
> `demo` uses an older version of `bevy_pretty_text` with a different syntax because 
> one of its dependencies is not updated.

`cargo run --bin demo`<br>

<img src="https://raw.githubusercontent.com/void-scape/bevy_pretty_text/v0.3.0/demo/assets/demo-screenshot.png" width="480" alt="Bevy game demo">

## Getting Started

First, add `bevy_pretty_text` to the dependencies in your `Cargo.toml`:
 
```toml
[dependencies]
bevy_pretty_text = "0.3"
```

Then, you'll need to add the `PrettyTextPlugin` to your app.

```rs
use bevy::prelude::*;
use bevy_pretty_text::prelude::*;

fn main() {
    App::default()
        .add_plugins((DefaultPlugins, PrettyTextPlugin))
        .run();
}
```

And then you can make some _pretty text_!

```rust
fn spawn_text(mut commands: Commands, mut materials: ResMut<Assets<Rainbow>>) {
    // Spawn wavy `Text`.
    commands.spawn((
        Text::new("Hello, World!"),
        Wave::default(),
        TextFont::from_font_size(52.0),
    ));

    // Use the typewriter.
    commands.spawn((
        Typewriter::new(30.),
        Text2d::new("My text is revealed one glyph at a time"),
        Transform::from_xyz(0., 200., 0.),
        TextFont::from_font_size(36.0),
    ));

    // Spawn a style entity.
    commands.spawn((
        PrettyStyle("my_style"),
        TextFont::from_font_size(48.0),
        effects![
            PrettyTextMaterial(materials.add(Rainbow::default())),
            Wave::default(),
        ],
    ));

    // Parse rich text and use custom style.
    commands.spawn((
        pretty!("I am |1|<0.8>*sniff*|1|<1.2> very [pretty](my_style)!|3|<1>"),
        Typewriter::new(10.0),
        TextFont::from_font_size(52.0),
        Node {
            position_type: PositionType::Absolute,
            left: Val::Px(0.0),
            bottom: Val::Px(0.0),
            ..Default::default()
        },
    ));
}
```

<img src="https://raw.githubusercontent.com/void-scape/bevy_pretty_text/v0.3.0/assets/simple.gif" width="480" alt="Example code demonstration">

---

[The repository examples](examples/src/bin) should help you get up to speed on common usage patterns.

## Feature flags

| Flag              | Description                                 | Default feature |
| ----------------- | ------------------------------------------- | --------------- |
| `serialize`       | Enable serialization for `ParsedPrettyText`.| No              |

## Bevy version compatibility

| `bevy` | `bevy_pretty_text` |
| ------ | ------------------ |
| 0.17   | 0.3                |
| 0.16   | 0.1-0.2            |

## License

Pretty Text is free and open source. All code in this repository is dual-licensed under either:

- MIT License ([LICENSE-MIT](/LICENSE-MIT) or <http://opensource.org/licenses/MIT>)
- Apache License, Version 2.0 ([LICENSE-APACHE](/LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)

at your option.
