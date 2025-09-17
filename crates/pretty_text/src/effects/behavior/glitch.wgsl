#import bevy_sprite::{
    mesh2d_functions as mesh_functions,
    mesh2d_view_bindings::view,
}

#import bevy_render::globals::Globals
@group(0) @binding(1) var<uniform> globals: Globals;

fn random(seed: vec2<f32>) -> f32 {
    return fract(sin(dot(seed, vec2<f32>(12.9898, 78.233))) * 43758.5453);
}

fn noise(p: vec2<f32>) -> f32 {
    let i = floor(p);
    let f = fract(p);
    let u = f * f * (3.0 - 2.0 * f);
    return mix(
        mix(random(i + vec2(0.0, 0.0)), random(i + vec2(1.0, 0.0)), u.x),
        mix(random(i + vec2(0.0, 1.0)), random(i + vec2(1.0, 1.0)), u.x),
        u.y
    );
}

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
};

@group(1) @binding(0) var texture: texture_2d<f32>;
@group(1) @binding(1) var texture_sampler: sampler;

@group(2) @binding(0) var<uniform> intensity: f32;
@group(2) @binding(1) var<uniform> frequency: f32;
@group(2) @binding(2) var<uniform> speed: f32;
@group(2) @binding(3) var<uniform> threshold: f32;


@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let time = globals.time;
    var uv = in.uv;
     
    let scanline = floor(uv.y * frequency);
    let noise_input = vec2(scanline * 0.1, time * speed);
    let glitch_noise = noise(noise_input);
    let secondary_noise = noise(vec2(scanline * 0.03, time * speed * 0.7));
    
    if (glitch_noise > threshold) {
        let displacement = (glitch_noise - threshold) / (1.0 - threshold);
        let displacement_amount = displacement * intensity;
        let final_displacement = displacement_amount * (secondary_noise - 0.5) * 2.0;
        
        uv.x += final_displacement;
        uv.x = fract(uv.x);
    }
    
    return textureSample(texture, texture_sampler, uv) * in.color;
}
