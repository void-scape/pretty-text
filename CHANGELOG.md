# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.1] - 2026-02-11

### Fixed

- wasm panic related to `Glitch` and `Rainbow` uniforms.

## [0.4.0] - 2026-01-17

### Fixed

- Panic when parsing empty strings.
- Build errors about missing method `with_docs` in complex projects.

### Changed 

- Upgraded to Bevy 0.18.
- `Rainbow` effect will ignore `TextColor`.

## [0.3.1] - 2025-10-17

### Fixed

- Panic when encountering newlines.

## [0.3.0] - 2025-10-13

### Added

- `vertex_mask` example.
- `dynamic_styles` example.

### Changed 

- Upgraded to Bevy 0.17
- Renamed system sets as per the ![Bevy 0.17 recommendation](https://bevy.org/news/bevy-0-17/#consistent-naming-conventions-for-system-sets):
    - `PrettyEffectSet` => `PrettyEffectSystems`
    - `PrettyStyleSet` => `PrettyStyleSystems`
    - `TypewriterSet` => `TypewriterSystems`
- Parser syntax. Spans are now delimited with brackets and styles with parenthesis: `[my span](my_styles)`.
  This syntax is more intuitive for new users and allows defining spans without styles, 
  e.g. `first, [second], third`.
- Renamed `PauseTypewriter` to `DelayTypewriter`. `PauseTypewriter` now pauses execution
  indefinitely.
- `GlyphScale` is now only computed from `TextFont::font_size`. The translation of `GlyphVertices` is 
  automatically scaled based on the `GlobalTransform`/`UiGlobalTransform` components.
- Pulled out shader bindings and types into a `bevy_pretty_text` shader library. See 
  `default_glyph_material.wgsl` for an example.
- `String` to `PrettyParserError` as the error type for parsing operations.
- Effects systems have been moved from `Update` to `PostUpdate`. This prevents any delay between 
  glyph and effects systems.

### Removed

- `default_styles` example because it relied on a non-obvious implementation detail.
- `TrackedSpan` because of the introduction of `SpawnDetails`.

### Fixed

- Rendering bug when `Text` and `Text2d` are used in the same scene.

## [0.2.0] - 2025-09-17

### Added

- Support for `Text`.
- Support for effects inserted directly into text entities.
- `PrettyTextParser2d` struct and `pretty2d` macro. `PrettyTextParser` and `pretty` now
  produce `Text` instead of `Text2d`.
- Custom render pipelines for `Text` and `Text2d` using `GlyphMaterial`s.
- New behavior effects: `Rainbow`, `Bounce`, `Breathe`, `Fade`, `Pivot`, and `Spin`.
- New appearance effects: `FadeIn` and `Spread`. Appearance effects are driven by 
  the `Appeared` component, which is automatically inserted into `Glyph` entities when 
  `GlyphRevealed` events are triggered by the `Typewriter`.
- `effects` macro to declare `EffectOf` relationships in style entities.
- `EffectQuery` to iterate over `Glyph` effect query data.
- Components to configure `Typewriter` behavior: `DisableCommands`, `DisableEvents`, 
  `DisableCallbacks`, `DisableAppearance`, `FinishTypewriter`, and `ShortCircuitTypewriter`.
- `PrettyTextBuilder` for dynamically building `ParsedPrettyText`.
- Tracking information for `ParsedPrettyText` to debug syntax and style errors.
- `Styles` component for text spans to track style entities.
- `StyleWriter` to update the styles in `Styles` components.
- Default style entities for all colors in `bevy::color::palettes::basic`.

### Changed

- Refactored crate structure. Moved `bevy_pretty_effects` and `bevy_pretty_text` crates 
  into `pretty_text`. Renamed `pretty_text` to `bevy_pretty_text`. Pulled out the pretty 
  text parser into the `pretty_text_parser` crate.
- Vertex shader input and view uniform binding for `GlyphMaterial`s. Look to 
  `default_glyph_material.wgsl` for reference.
- `Scramble` is now an appearance effect instead of a behavior effect.
- Renamed `PrettyTextSpans` to `ParsedPrettyText`.
- Replaced `GlyphSpanEntity` with the `SpanGlyphs` and `SpanGlyphOf` relationship.
- Renamed the `TextMaterial2d` trait to `GlyphMaterial`. Removed the `set_atlas` method 
  and added `vertex_shader` and `fragment_shader` methods.
- Renamed `DynamicTextMaterial` to `DynamicEffect`
- Renamed `ErasedPrettyTextMaterial` to `Style`.
- Renamed `TypeWriterMode` to `TypewriterIndex`.

### Removed

- `Typewriter::finish`. Insert the `FinishTypewriter` component instead.
- `Reveal` component. `Glyph` visibility is now managed with the `GlyphRevealed` event.
- `GlyphOffset`. Replaced functionality with `GlyphVertices`.
- `GlyphOrigin`. `Glyph` entities now automatically calculate their position.
- `GlyphCache` and `GlyphCacheTrimTimeout`. `Glyph` entities are directly extracted in 
  the new render pipelines, removing the need to cache `Mesh`es.
- `SpanAtlasImage`. The atlas image is automatically provided for shader code in the 
  new render pipelines.
- `default_effects` feature. Effects have been migrated to the `bevy_pretty_text` crate.
- `TextMaterial2d` derive macro.

### Fixed

- Replaced all names with `TypeWriter` to `Typewriter`.
