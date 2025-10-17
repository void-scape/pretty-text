#define_import_path bevy_pretty_text

#import bevy_render::{
    view::View,
    globals::Globals,
}

@group(0) @binding(0) var<uniform> view: View;
@group(0) @binding(1) var<uniform> globals: Globals;

@group(1) @binding(0) var atlas_texture: texture_2d<f32>;
@group(1) @binding(1) var atlas_sampler: sampler;

// Vertex shader input for the `GlyphMaterial` pipeline.
struct VertexInput {
    // vertex-rate fields
    // extracted from the `GlyphVertices` component
    @location(0) position: vec3<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) color: vec4<f32>,

    // instance-rate fields
    @location(3) span_color: vec4<f32>,
    // scalar derived from font_size
    @location(4) size: f32,
    // glyph index in the text block
    @location(5) index: u32,
};

// Output of the default shader in the `GlyphMaterial` pipeline.
struct VertexOutput {
    // clip_position
    @builtin(position) position: vec4<f32>,
    // interpolated uv
    @location(0) uv: vec2<f32>,
    // interpolated color, mix of span and glyph color
    @location(1) color: vec4<f32>,
    // scalar derived from font_size
    @location(2) size: f32,
    // glyph index in the text block
    @location(3) index: u32,
};
