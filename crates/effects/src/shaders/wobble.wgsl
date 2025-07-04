#import bevy_sprite::{
    mesh2d_functions as mesh_functions,
    mesh2d_view_bindings::view,
}
#import bevy_render::globals::Globals

@group(0) @binding(1) var<uniform> globals: Globals;

struct Vertex {
    @builtin(instance_index) instance_index: u32,
    @location(0) position: vec3<f32>,
    @location(1) atlas_uv: vec3<f32>,
    @location(2) uv: vec2<f32>,
    @location(4) color: vec4<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) atlas_uv: vec2<f32>,
    @location(2) color: vec4<f32>,
};

@group(2) @binding(2) var<uniform> intensity: f32;

@vertex
fn vertex(vertex: Vertex) -> VertexOutput {
    var out: VertexOutput;
    var world_from_local = mesh_functions::get_world_from_local(vertex.instance_index);
    
    let world_position = mesh_functions::mesh2d_position_local_to_world(
        world_from_local,
        vec4<f32>(vertex.position, 1.0)
    );
    let time_factor = globals.time * 8.0;
    let instance_seed = f32(vertex.instance_index) * 0.1;
    
    let shake_x = sin(time_factor + instance_seed) * cos(time_factor * 1.3 + instance_seed * 2.0);
    let shake_y = cos(time_factor + instance_seed) * sin(time_factor * 1.7 + instance_seed * 3.0);
    
    let shake_offset = vec2(shake_x, shake_y) * intensity * 20.0;
    out.position = mesh_functions::mesh2d_position_world_to_clip(world_position + vec4(shake_offset, 0.0, 0.0));
    out.uv = vertex.uv;
    out.atlas_uv = vertex.atlas_uv.xy;
    out.color = vertex.color;
    
    return out;
}
