#import bevy_sprite::{
    mesh2d_functions as mesh_functions,
    mesh2d_view_bindings::view,
}

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
#else
@group(2) @binding(0) var texture: texture_2d<f32>;
@group(2) @binding(1) var texture_sampler: sampler;
@group(2) @binding(2) var<uniform> speed: f32;
#endif

// https://www.shadertoy.com/view/4dKcWK
fn hsv_to_linear(hsv: vec3<f32>) -> vec3<f32> {
    let rgb = abs(hsv.x * 6. - vec3(3, 2, 4)) * vec3(1, -1, -1) + vec3(-1, 2, 2);
    let clamped = clamp(rgb, vec3(0.), vec3(1.));
    return ((clamped - 1.) * hsv.y + 1.) * hsv.z;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let hue = fract(in.uv.x + in.uv.y + globals.time * speed * 0.5);

    let t = textureSample(texture, texture_sampler, in.uv);
    let rgb = hsv_to_linear(vec3<f32>(hue, 1.0, 1.0));
    let rainbow = vec4<f32>(rgb, 1.0);
    var out = rainbow;
    out.a = t.a * in.color.a;

    return out;
}
