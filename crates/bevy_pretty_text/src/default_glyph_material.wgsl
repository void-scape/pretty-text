#ifdef TONEMAP_IN_SHADER
#import bevy_core_pipeline::tonemapping
#endif

#import bevy_pretty_text::{
    view,
    globals,
    atlas_texture,
    atlas_sampler,
    VertexInput,
    VertexOutput,
}

@vertex
fn vertex(vertex: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    out.position = view.clip_from_world * vec4<f32>(vertex.position, 1.0);
    out.uv = vertex.uv;
    // vertex.color acts like a color mask here
    out.color = vertex.color * vertex.span_color;
    out.size = vertex.size;
    out.index = vertex.index;

    return out;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    var color = in.color * textureSample(atlas_texture, atlas_sampler, in.uv);

#ifdef TONEMAP_IN_SHADER
    color = tonemapping::tone_mapping(color, view.color_grading);
#endif

    return color;
}
