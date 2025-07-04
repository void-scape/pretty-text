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
@group(2) @binding(3) var<uniform> max_height: f32;

@vertex
fn vertex(vertex: Vertex) -> VertexOutput {
    var out: VertexOutput;

    var world_from_local = mesh_functions::get_world_from_local(vertex.instance_index);
    let quad_center = mesh_functions::mesh2d_position_local_to_world(
        world_from_local,
        vec4<f32>(0.0, 0.0, 0.0, 1.0)
    );
    
    let time_factor = globals.time * intensity;
    let wave = sin(quad_center.x * 0.02 + time_factor * 2.0) * 0.4;
    let wave_offset = wave * max_height;
    
    let offset_transform = mat4x4(
        vec4(1.0, 0.0, 0.0, 0.0),
        vec4(0.0, 1.0, 0.0, 0.0),
        vec4(0.0, 0.0, 1.0, 0.0),
        vec4(0.0, wave_offset, 0.0, 1.0)
    );
    let final_transform = offset_transform * world_from_local;
    let final_world_position = mesh_functions::mesh2d_position_local_to_world(
        final_transform,
        vec4(vertex.position, 1.0)
    );
    
    out.position = mesh_functions::mesh2d_position_world_to_clip(final_world_position);
    out.uv = vertex.uv;
    out.atlas_uv = vertex.atlas_uv.xy;
    out.color = vertex.color;
    
    return out;
}
