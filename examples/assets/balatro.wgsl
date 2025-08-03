// This was originally an imitation of Balatro's paint effect (https://www.shadertoy.com/view/XXtBRr)
//
// Ported to WGSL

#import bevy_sprite::mesh2d_vertex_output::VertexOutput;
#import bevy_render::globals::Globals;
@group(0) @binding(1) var<uniform> globals: Globals;

const SPIN_ROTATION: f32 = -0.5;
const SPIN_SPEED: f32 = 2.0;
const OFFSET: vec2f = vec2f(0.0, 0.0);

const COLOUR_1: vec4f = vec4f(0.27, 0.29, 0.29, 1.0);
const COLOUR_2: vec4f = vec4f(0.184, 0.207, 0.196, 1.0);
const COLOUR_3: vec4f = vec4f(0.101, 0.121, 0.113, 1.0);

const CONTRAST: f32 = 2.5;
const LIGHTING: f32 = 0.1;
const SPIN_AMOUNT: f32 = 0.15;
const PIXEL_FILTER: f32 = 745.0;
const SPIN_EASE: f32 = 0.1;
const PI: f32 = 3.14159265359;
const IS_ROTATE: bool = false;

fn effect(screenSize: vec2f, screen_coords: vec2f) -> vec4f {
    let pixel_size = length(screenSize) / PIXEL_FILTER;
    var uv = (floor(screen_coords * (1.0 / pixel_size)) * pixel_size - 0.5 * screenSize) / length(screenSize) - OFFSET;
    let uv_len = length(uv);
    
    var speed = SPIN_ROTATION * SPIN_EASE * 0.2;
    if (IS_ROTATE) {
        speed = globals.time * speed;
    }
    speed += 302.2;
    
    let new_pixel_angle = atan2(uv.y, uv.x) + speed - SPIN_EASE * 20.0 * (1.0 * SPIN_AMOUNT * uv_len + (1.0 - 1.0 * SPIN_AMOUNT));
    let mid = (screenSize / length(screenSize)) / 2.0;
    uv = (vec2f((uv_len * cos(new_pixel_angle) + mid.x), (uv_len * sin(new_pixel_angle) + mid.y)) - mid);
    
    uv *= 30.0;
    speed = globals.time * SPIN_SPEED;
    var uv2 = vec2f(uv.x + uv.y);
    
    for (var i = 0; i < 5; i++) {
        uv2 += sin(max(uv.x, uv.y)) + uv;
        uv += 0.5 * vec2f(cos(5.1123314 + 0.353 * uv2.y + speed * 0.131121), sin(uv2.x - 0.113 * speed));
        uv -= 1.0 * cos(uv.x + uv.y) - 1.0 * sin(uv.x * 0.711 - uv.y);
    }
    
    let contrast_mod = (0.25 * CONTRAST + 0.5 * SPIN_AMOUNT + 1.2);
    let paint_res = min(2.0, max(0.0, length(uv) * 0.035 * contrast_mod));
    let c1p = max(0.0, 1.0 - contrast_mod * abs(1.0 - paint_res));
    let c2p = max(0.0, 1.0 - contrast_mod * abs(paint_res));
    let c3p = 1.0 - min(1.0, c1p + c2p);
    let light = (LIGHTING - 0.2) * max(c1p * 5.0 - 4.0, 0.0) + LIGHTING * max(c2p * 5.0 - 4.0, 0.0);
    
    return to_linear((0.3 / CONTRAST) * COLOUR_1 + (1.0 - 0.3 / CONTRAST) * (COLOUR_1 * c1p + COLOUR_2 * c2p + vec4f(c3p * COLOUR_3.rgb, c3p * COLOUR_1.a)) + light);
}

// https://github.com/bevyengine/bevy/discussions/8937
fn to_linear(nonlinear: vec4f) -> vec4f {
    let cutoff = step(nonlinear, vec4f(0.04045));
    let higher = pow((nonlinear + vec4f(0.055)) / vec4f(1.055), vec4f(2.4));
    let lower = nonlinear / vec4f(12.92);
    return mix(higher, lower, cutoff);
}

@fragment
fn fragment(
    vertex: VertexOutput,
) -> @location(0) vec4f {
    let resolution = vec2f(1280, 720);
    return effect(resolution, vertex.uv * resolution);
}
