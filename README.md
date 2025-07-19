<img src="https://media3.giphy.com/media/v1.Y2lkPTc5MGI3NjExMGN2OTZrcjc5ZGczbXdiZWxidGNndmI5cjI4b2RibWJqcWJ0MHNiZyZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/k775K1Qcy9VXxKB3nG/giphy.gif" width="100%"/>

[![crates.io](https://img.shields.io/crates/v/bevy_pretty_text)](https://crates.io/crates/bevy_pretty_text)
[![docs.rs](https://docs.rs/bevy_pretty_text/badge.svg)](https://docs.rs/bevy_pretty_text)

**Pretty Text** is a Text2d effects library for [Bevy](https://bevyengine.org/).

## Demos

`cargo run --example type_writer`<br>

![A type writer demonstration](https://media3.giphy.com/media/v1.Y2lkPTc5MGI3NjExZWxtejczd2ExZTNldnFnY2V6cnB5MnBpdWp4eXp4dTNhanMxbmZ0aiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/ddsSae3YSASTARzKne/giphy.gif)

---

`cargo run --example effects`<br>

![Various text effects](https://media3.giphy.com/media/v1.Y2lkPTc5MGI3NjExcXYwbThubXFnbW5yM3piamd3a3hlMzY3MjE4c283Z3hxNmx0M2hxbCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/PzWZ4orUZoPjAaV7Cp/giphy.gif)

---

The Bevy demo game is in `demo` and can be run by using

```sh
cargo run --bin demo
```

## Getting Started

First, add `bevy_pretty_text` to the dependencies in your `Cargo.toml`:
 
```toml
[dependencies]
bevy_pretty_text = "0.1"
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

```rs
fn spawn_text(mut commands: Commands) {
    // Spawn text.
    commands.spawn(pretty!("I am very `pretty`[wave, !green]!"));

    // Spawn type writer text.
    commands
        .spawn((
            TypeWriter::new(30.),
            pretty!("I am [1]<0.8>*sniff*[1]<1.2> very `pretty`[wave, !green]![3]<1>"),
            Transform::from_xyz(0., 200., 0.),
        ))
        .observe(
            |trigger: Trigger<TypeWriterFinished>, mut commands: Commands| {
                commands
                    .entity(trigger.target())
                    .insert(TypeWriter::new(30.));
            },
        );
}
```

[The repository’s examples](https://github.com/void-scape/pretty-text/tree/a0a0a5631b9302d1db292b9e19d6955809835633/crates/plugin/examples) should help you get up to speed on common usage patterns.

## Bevy version compatibility

| `bevy` | `bevy_pretty_text` |
| ------ | ------------------ |
| 0.16   | 0.1                |

## License

Pretty Text is free and open source. All code in this repository is dual-licensed under either:

- MIT License ([LICENSE-MIT](/LICENSE-MIT) or <http://opensource.org/licenses/MIT>)
- Apache License, Version 2.0 ([LICENSE-APACHE](/LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)

at your option.
