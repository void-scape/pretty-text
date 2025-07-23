#import bevy_render::{
    view::View,
    globals::Globals,
}

// The Vertex output of the default vertex shader for the Ui Material pipeline.
struct UiVertexOutput {
    @location(0) uv: vec2<f32>,
    // The size of the borders in UV space. Order is Left, Right, Top, Bottom.
    @location(1) border_widths: vec4<f32>,
    // The size of the borders in pixels. Order is top left, top right, bottom right, bottom left.
    @location(2) border_radius: vec4<f32>,
    // The size of the node in pixels. Order is width, height.
    @location(3) @interpolate(flat) size: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@group(0) @binding(0) var<uniform> view: View;
@group(0) @binding(1) var<uniform> globals: Globals;

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>,
    @location(2) size: vec2<f32>,
    @location(3) border_widths: vec4<f32>,
    @location(4) border_radius: vec4<f32>,
) -> UiVertexOutput {
    var out: UiVertexOutput;
    out.uv = vertex_uv;
    out.position = view.clip_from_world * vec4<f32>(vertex_position, 1.0);
    out.size = size;
    out.border_widths = border_widths;
    out.border_radius = border_radius;
    return out;
}

@fragment
fn fragment(in: UiVertexOutput) -> @location(0) vec4<f32> {
    return vec4<f32>(1.0);
}
