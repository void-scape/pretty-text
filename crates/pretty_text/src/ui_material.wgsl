#import bevy_render::{
    view::View,
    globals::Globals,
}

struct GlyphVertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) @interpolate(flat) size: vec2<f32>,
    @location(3) color: vec4<f32>,
};

@group(0) @binding(0) var<uniform> view: View;
@group(0) @binding(1) var<uniform> globals: Globals;

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>,
    @location(2) size: vec2<f32>,
    @location(3) color: vec4<f32>,
) -> GlyphVertexOutput {
    var out: GlyphVertexOutput;
    out.uv = vertex_uv;
    out.position = view.clip_from_world * vec4<f32>(vertex_position, 1.0);
    out.size = size;
    out.color = color;
    return out;
}

@fragment
fn fragment(in: GlyphVertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
