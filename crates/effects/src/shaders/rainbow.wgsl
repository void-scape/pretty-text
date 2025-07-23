#import bevy_sprite::{
    mesh2d_functions as mesh_functions,
    mesh2d_view_bindings::view,
}
#import bevy_render::color_operations::hsv_to_rgb;

#import bevy_render::globals::Globals
@group(0) @binding(1) var<uniform> globals: Globals;

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
};

#ifdef TEXT_UI
@group(1) @binding(0) var texture: texture_2d<f32>;
@group(1) @binding(1) var texture_sampler: sampler;
@group(1) @binding(2) var<uniform> speed: f32;
@group(1) @binding(3) var<uniform> width: f32;
#else
@group(2) @binding(0) var texture: texture_2d<f32>;
@group(2) @binding(1) var texture_sampler: sampler;
@group(2) @binding(2) var<uniform> speed: f32;
@group(2) @binding(3) var<uniform> width: f32;
#else
#endif

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    var color = in.color * textureSample(texture, texture_sampler, in.uv);
    let w = 1.0 / (90.0 * width);
    color *= vec4(hsv_to_rgb(vec3(in.position.x * w + 2.0 * speed * globals.time, 1.0, 0.5)), 1.0);
    return color;
}
