#ifdef TONEMAP_IN_SHADER
#import bevy_core_pipeline::tonemapping
#endif

#import bevy_render::{
    view::View,
    globals::Globals,
}

// Vertex shader input for the `GlyphMaterial` pipeline.
struct Vertex {
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

@group(0) @binding(0) var<uniform> view: View;
@group(0) @binding(1) var<uniform> globals: Globals;

@vertex
fn vertex(vertex: Vertex) -> VertexOutput {
    var out: VertexOutput;

    out.position = view.clip_from_world * vec4<f32>(vertex.position, 1.0);
    out.uv = vertex.uv;
    // vertex.color acts like a color mask here
    out.color = vertex.color * vertex.span_color;
    out.size = vertex.size;
    out.index = vertex.index;

    return out;
}

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

@group(1) @binding(0) var texture: texture_2d<f32>;
@group(1) @binding(1) var texture_sampler: sampler;

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    var color = in.color * textureSample(texture, texture_sampler, in.uv);

#ifdef TONEMAP_IN_SHADER
    color = tonemapping::tone_mapping(color, view.color_grading);
#endif

    return color;
}
