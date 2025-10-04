use core::marker::PhantomData;
use std::hash::Hash;

use bevy::core_pipeline::core_2d::{CORE_2D_DEPTH_FORMAT, Transparent2d};
use bevy::core_pipeline::tonemapping::{DebandDither, Tonemapping};
use bevy::ecs::query::ROQueryItem;
use bevy::ecs::system::lifetimeless::{Read, SRes};
use bevy::math::{FloatOrd, Mat4, Vec2};
use bevy::render::globals::GlobalsUniform;
use bevy::render::render_asset::{PrepareAssetError, RenderAsset, RenderAssetPlugin};
use bevy::render::render_resource::binding_types::uniform_buffer;
use bevy::render::sync_world::TemporaryRenderEntity;
use bevy::render::{
    Extract, Render,
    globals::GlobalsBuffer,
    render_asset::RenderAssets,
    render_phase::*,
    render_resource::*,
    renderer::{RenderDevice, RenderQueue},
    view::*,
};
use bevy::render::{RenderApp, RenderStartup, RenderSystems};
use bevy::shader::{ShaderDefVal, ShaderRef};
use bevy::sprite::Anchor;
use bevy::sprite_render::SpritePipelineKey;
use bevy::text::{ComputedTextBlock, PositionedGlyph, TextBounds, TextLayoutInfo};
use bevy::transform::prelude::GlobalTransform;
use bevy::{ecs::system::*, render::texture::GpuImage};

use crate::effects::material::{DEFAULT_GLYPH_SHADER_HANDLE, GlyphMaterial};
use crate::glyph::{Glyph, GlyphIndex, GlyphScale, GlyphVertices, Glyphs, SpanGlyphOf};
use crate::render::{GlyphInstance, GlyphVertex};
use crate::*;

use super::{
    DrawGlyph, ExtractedGlyph, ExtractedGlyphSpan, ExtractedGlyphSpanKind, ExtractedGlyphSpans,
    ExtractedGlyphs, GlyphBatch, GlyphMaterialMeta, ImageBindGroups, SetTextureBindGroup,
};

const QUAD_VERTEX_POSITIONS: [Vec2; 4] = [
    Vec2::new(-0.5, -0.5),
    Vec2::new(0.5, -0.5),
    Vec2::new(0.5, 0.5),
    Vec2::new(-0.5, 0.5),
];

const QUAD_INDICES: [usize; 6] = [0, 2, 3, 0, 1, 2];

pub(super) struct GlyphMaterial2dPlugin<T: GlyphMaterial>(PhantomData<T>);

impl<T: GlyphMaterial> Default for GlyphMaterial2dPlugin<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: GlyphMaterial> Plugin for GlyphMaterial2dPlugin<T>
where
    T::Data: PartialEq + Eq + Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.init_asset::<T>()
            .add_plugins(RenderAssetPlugin::<PreparedGlyphMaterial2d<T>>::default());

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_render_command::<Transparent2d, DrawGlyphMaterial2d<T>>()
                .init_resource::<SpecializedRenderPipelines<GlyphMaterial2dPipeline<T>>>()
                .add_systems(RenderStartup, init_glyph_material_ui_pipeline::<T>)
                .add_systems(
                    Render,
                    (
                        queue_glyphs::<T>.in_set(RenderSystems::Queue),
                        prepare_view_bind_groups::<T>.in_set(RenderSystems::PrepareBindGroups),
                        prepare_glyphs::<T>
                            .in_set(RenderSystems::PrepareBindGroups)
                            .after(prepare_view_bind_groups::<T>)
                            .after(queue_glyphs::<T>),
                    ),
                );
        }
    }
}

#[derive(Resource)]
pub(super) struct GlyphMaterial2dPipeline<M: GlyphMaterial> {
    view_layout: BindGroupLayout,
    texture_layout: BindGroupLayout,
    material_layout: BindGroupLayout,
    vertex_shader: Option<Handle<Shader>>,
    fragment_shader: Option<Handle<Shader>>,
    marker: PhantomData<M>,
}

fn init_glyph_material_ui_pipeline<M: GlyphMaterial>(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    render_device: Res<RenderDevice>,
) {
    let view_layout = render_device.create_bind_group_layout(
        "pretty_text_2d_view_layout",
        &BindGroupLayoutEntries::sequential(
            ShaderStages::VERTEX_FRAGMENT,
            (
                uniform_buffer::<ViewUniform>(true),
                uniform_buffer::<GlobalsUniform>(false),
            ),
        ),
    );
    let texture_layout = render_device.create_bind_group_layout(
        "pretty_text_texture_layout",
        &BindGroupLayoutEntries::sequential(
            ShaderStages::FRAGMENT,
            (
                binding_types::texture_2d(TextureSampleType::Float { filterable: true }),
                binding_types::sampler(SamplerBindingType::Filtering),
            ),
        ),
    );
    let material_layout = M::bind_group_layout(&render_device);

    commands.insert_resource(GlyphMaterial2dPipeline {
        view_layout,
        texture_layout,
        material_layout,
        vertex_shader: match M::vertex_shader() {
            ShaderRef::Default => None,
            ShaderRef::Handle(handle) => Some(handle),
            ShaderRef::Path(path) => Some(asset_server.load(path)),
        },
        fragment_shader: match M::fragment_shader() {
            ShaderRef::Default => None,
            ShaderRef::Handle(handle) => Some(handle),
            ShaderRef::Path(path) => Some(asset_server.load(path)),
        },
        marker: PhantomData::<M>,
    });
}

impl<M: GlyphMaterial> SpecializedRenderPipeline for GlyphMaterial2dPipeline<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    type Key = SpritePipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let mut shader_defs = Vec::new();
        if key.contains(SpritePipelineKey::TONEMAP_IN_SHADER) {
            shader_defs.push("TONEMAP_IN_SHADER".into());
            shader_defs.push(ShaderDefVal::UInt(
                "TONEMAPPING_LUT_TEXTURE_BINDING_INDEX".into(),
                1,
            ));
            shader_defs.push(ShaderDefVal::UInt(
                "TONEMAPPING_LUT_SAMPLER_BINDING_INDEX".into(),
                2,
            ));

            let method = key.intersection(SpritePipelineKey::TONEMAP_METHOD_RESERVED_BITS);

            if method == SpritePipelineKey::TONEMAP_METHOD_NONE {
                shader_defs.push("TONEMAP_METHOD_NONE".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_REINHARD {
                shader_defs.push("TONEMAP_METHOD_REINHARD".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_REINHARD_LUMINANCE {
                shader_defs.push("TONEMAP_METHOD_REINHARD_LUMINANCE".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_ACES_FITTED {
                shader_defs.push("TONEMAP_METHOD_ACES_FITTED".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_AGX {
                shader_defs.push("TONEMAP_METHOD_AGX".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_SOMEWHAT_BORING_DISPLAY_TRANSFORM
            {
                shader_defs.push("TONEMAP_METHOD_SOMEWHAT_BORING_DISPLAY_TRANSFORM".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_BLENDER_FILMIC {
                shader_defs.push("TONEMAP_METHOD_BLENDER_FILMIC".into());
            } else if method == SpritePipelineKey::TONEMAP_METHOD_TONY_MC_MAPFACE {
                shader_defs.push("TONEMAP_METHOD_TONY_MC_MAPFACE".into());
            }

            // Debanding is tied to tonemapping in the shader, cannot run without it.
            if key.contains(SpritePipelineKey::DEBAND_DITHER) {
                shader_defs.push("DEBAND_DITHER".into());
            }
        }

        let format = match key.contains(SpritePipelineKey::HDR) {
            true => ViewTarget::TEXTURE_FORMAT_HDR,
            false => TextureFormat::bevy_default(),
        };

        let buffers = super::vertex_buffer_layouts().to_vec();

        let mut descriptor = RenderPipelineDescriptor {
            vertex: VertexState {
                shader: DEFAULT_GLYPH_SHADER_HANDLE,
                entry_point: Some("vertex".into()),
                shader_defs: shader_defs.clone(),
                buffers,
            },
            fragment: Some(FragmentState {
                shader: DEFAULT_GLYPH_SHADER_HANDLE,
                shader_defs,
                entry_point: Some("fragment".into()),
                targets: vec![Some(ColorTargetState {
                    format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            layout: vec![
                self.view_layout.clone(),
                self.texture_layout.clone(),
                self.material_layout.clone(),
            ],
            push_constant_ranges: Vec::new(),
            primitive: PrimitiveState::default(),
            depth_stencil: Some(DepthStencilState {
                format: CORE_2D_DEPTH_FORMAT,
                depth_write_enabled: false,
                depth_compare: CompareFunction::GreaterEqual,
                stencil: StencilState {
                    front: StencilFaceState::IGNORE,
                    back: StencilFaceState::IGNORE,
                    read_mask: 0,
                    write_mask: 0,
                },
                bias: DepthBiasState {
                    constant: 0,
                    slope_scale: 0.0,
                    clamp: 0.0,
                },
            }),
            multisample: MultisampleState {
                count: key.msaa_samples(),
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            label: Some("pretty_text_2d_pipeline".into()),
            zero_initialize_workgroup_memory: false,
        };

        if let Some(vertex_shader) = &self.vertex_shader {
            descriptor.vertex.shader = vertex_shader.clone();
        }

        if let Some(fragment_shader) = &self.fragment_shader {
            descriptor.fragment.as_mut().unwrap().shader = fragment_shader.clone();
        }

        descriptor
    }
}

pub(super) type DrawGlyphMaterial2d<M> = (
    SetItemPipeline,
    SetViewBindGroup<M, 0>,
    SetTextureBindGroup<M, 1>,
    SetMaterialBindGroup<M, 2>,
    DrawGlyph<M>,
);

pub(super) struct SetViewBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P> for SetViewBindGroup<M, I> {
    type Param = ();
    type ViewQuery = (Read<ViewUniformOffset>, Read<Glyph2dViewBindGroup>);
    type ItemQuery = ();

    fn render<'w>(
        _item: &P,
        (view_uniform, bind_group): ROQueryItem<'w, '_, Self::ViewQuery>,
        _entity: Option<()>,
        _param: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        pass.set_bind_group(I, &bind_group.0, &[view_uniform.offset]);
        RenderCommandResult::Success
    }
}

pub(super) struct SetMaterialBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P>
    for SetMaterialBindGroup<M, I>
{
    type Param = SRes<RenderAssets<PreparedGlyphMaterial2d<M>>>;
    type ViewQuery = ();
    type ItemQuery = Read<GlyphBatch<M>>;

    fn render<'w>(
        _item: &P,
        _view: (),
        material_handle: Option<ROQueryItem<'_, '_, Self::ItemQuery>>,
        materials: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(material_handle) = material_handle else {
            return RenderCommandResult::Skip;
        };
        let Some(material) = materials.into_inner().get(material_handle.material) else {
            return RenderCommandResult::Skip;
        };
        pass.set_bind_group(I, &material.bind_group, &[]);
        RenderCommandResult::Success
    }
}

#[derive(Component)]
pub(super) struct Glyph2dViewBindGroup(BindGroup);

fn prepare_view_bind_groups<T: GlyphMaterial>(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    pipeline: Res<GlyphMaterial2dPipeline<T>>,
    view_uniforms: Res<ViewUniforms>,
    globals_buffer: Res<GlobalsBuffer>,
    views: Query<Entity, With<ExtractedView>>,
) {
    let Some(view_binding) = view_uniforms.uniforms.binding() else {
        return;
    };
    let Some(globals_binding) = globals_buffer.buffer.binding() else {
        return;
    };

    for entity in &views {
        let view_bind_group = render_device.create_bind_group(
            "pretty_text_2d_view_bind_group",
            &pipeline.view_layout,
            &BindGroupEntries::sequential((view_binding.clone(), globals_binding.clone())),
        );

        commands
            .entity(entity)
            .insert(Glyph2dViewBindGroup(view_bind_group));
    }
}

pub(super) struct PreparedGlyphMaterial2d<T: GlyphMaterial> {
    _bindings: BindingResources,
    bind_group: BindGroup,
    _marker: PhantomData<T>,
}

impl<M: GlyphMaterial> RenderAsset for PreparedGlyphMaterial2d<M> {
    type SourceAsset = M;

    type Param = (
        SRes<RenderDevice>,
        SRes<GlyphMaterial2dPipeline<M>>,
        M::Param,
    );

    fn prepare_asset(
        material: Self::SourceAsset,
        _: AssetId<Self::SourceAsset>,
        (render_device, pipeline, material_param): &mut SystemParamItem<Self::Param>,
        _previous_asset: Option<&Self>,
    ) -> Result<Self, PrepareAssetError<Self::SourceAsset>> {
        match material.as_bind_group(&pipeline.material_layout, render_device, material_param) {
            Ok(prepared) => Ok(PreparedGlyphMaterial2d {
                _bindings: prepared.bindings,
                bind_group: prepared.bind_group,
                _marker: PhantomData,
            }),
            Err(AsBindGroupError::RetryNextUpdate) => {
                Err(PrepareAssetError::RetryNextUpdate(material))
            }
            Err(other) => Err(PrepareAssetError::AsBindGroupError(other)),
        }
    }
}

pub fn extract_glyphs(
    mut commands: Commands,
    mut extracted_spans: ResMut<ExtractedGlyphSpans>,
    mut extracted_glyphs: ResMut<ExtractedGlyphs>,
    texture_atlases: Extract<Res<Assets<TextureAtlasLayout>>>,
    text_query: Extract<
        Query<
            (
                Entity,
                &InheritedVisibility,
                &TextLayoutInfo,
                &TextBounds,
                &Anchor,
                &GlobalTransform,
                &Glyphs,
            ),
            With<ComputedTextBlock>,
        >,
    >,
    glyphs: Extract<
        Query<(
            &Glyph,
            &SpanGlyphOf,
            &InheritedVisibility,
            &GlyphScale,
            &GlyphIndex,
            &GlyphVertices,
        )>,
    >,
    text_styles: Extract<Query<&TextColor>>,
) {
    let mut index = extracted_glyphs.len();
    let mut extracted = Vec::new();

    for (
        entity,
        inherited_visibility,
        layout_info,
        bounds,
        anchor,
        global_transform,
        glyph_entities,
    ) in &text_query
    {
        let scaling =
            GlobalTransform::from_scale(Vec2::splat(layout_info.scale_factor.recip()).extend(1.));

        // NOTE: glyphs should not be clipped based on their view visibility because
        // we do not know if the glyph vertices are on the screen or not.
        //
        // Also, the view visibility is set to false so that the default text2d
        // renderer skips pretty text.
        if !inherited_visibility.get() {
            continue;
        }

        let size = Vec2::new(
            bounds.width.unwrap_or(layout_info.size.x),
            bounds.height.unwrap_or(layout_info.size.y),
        );
        let top_left = (Anchor::TOP_LEFT.0 - anchor.as_vec()) * size;
        let transform =
            *global_transform * GlobalTransform::from_translation(top_left.extend(0.)) * scaling;
        let model_matrix = transform.to_matrix();

        let mut iter = glyphs.iter_many(glyph_entities.iter()).peekable();

        while let Some((
            Glyph(PositionedGlyph {
                position,
                atlas_info,
                span_index,
                ..
            }),
            span_entity,
            inherited_visibility,
            glyph_scale,
            glyph_index,
            glyph_vertices,
        )) = iter.next()
        {
            if inherited_visibility.get() {
                let rect = texture_atlases
                    .get(atlas_info.texture_atlas)
                    .unwrap()
                    .textures[atlas_info.location.glyph_index]
                    .as_rect();
                extracted_glyphs.push(ExtractedGlyph {
                    vertices: glyph_vertices.0.map(|v| {
                        model_matrix
                            * Mat4::from_translation(Vec3::new(position.x, -position.y, 0.0))
                            * v.compute_transform().compute_affine()
                    }),
                    colors: glyph_vertices.0.map(|v| v.color.to_linear().to_f32_array()),
                    rect,
                    glyph_scale: glyph_scale.0,
                    index: glyph_index.0 as u32,
                });
                extracted.push(index);
                index += 1;
            }

            if !extracted.is_empty()
                && iter.peek().is_none_or(|(glyph, _, _, _, _, _)| {
                    glyph.0.span_index != *span_index
                        || glyph.0.atlas_info.texture != atlas_info.texture
                })
            {
                let color = text_styles
                    .get(span_entity.0)
                    .map(|text_color| LinearRgba::from(text_color.0))
                    .unwrap_or_default();

                extracted_spans.push(ExtractedGlyphSpan {
                    kind: ExtractedGlyphSpanKind::Sprite,
                    sork_key: transform.translation().z,
                    main_entity: entity.into(),
                    span_entity: span_entity.0.into(),
                    render_entity: commands.spawn(TemporaryRenderEntity).id(),
                    color: color.to_f32_array(),
                    image: atlas_info.texture,
                    extracted: std::mem::take(&mut extracted),
                    material_extracted: false,
                });
            }
        }
    }
}

fn prepare_glyphs<T: GlyphMaterial>(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    mut image_bind_groups: ResMut<ImageBindGroups>,
    glyph_meta: ResMut<GlyphMaterialMeta<T>>,
    extracted_spans: ResMut<ExtractedGlyphSpans>,
    extracted_glyphs: ResMut<ExtractedGlyphs>,
    view_uniforms: Res<ViewUniforms>,
    globals_buffer: Res<GlobalsBuffer>,
    material_pipeline: Res<GlyphMaterial2dPipeline<T>>,
    gpu_images: Res<RenderAssets<GpuImage>>,
    mut phases: ResMut<ViewSortedRenderPhases<Transparent2d>>,
    mut previous_len: Local<usize>,
) {
    if glyph_meta.materials.is_empty() {
        return;
    }

    if view_uniforms.uniforms.binding().is_some() && globals_buffer.buffer.binding().is_some() {
        let mut batches: Vec<(Entity, GlyphBatch<T>)> = Vec::with_capacity(*previous_len);
        let mut index = glyph_meta.instances.len() as u32;

        let glyph_meta = glyph_meta.into_inner();
        for phase in phases.values_mut() {
            for item_index in 0..phase.items.len() {
                let item = &mut phase.items[item_index];

                let Some(span_material) = glyph_meta.materials.get(&item.extracted_index) else {
                    continue;
                };

                let Some(span) = extracted_spans.0.get(item.extracted_index).filter(|span| {
                    span.render_entity == item.entity()
                        && matches!(span.kind, ExtractedGlyphSpanKind::Sprite)
                }) else {
                    continue;
                };

                let image = gpu_images
                    .get(span.image)
                    .expect("Image was checked during batching and should still exist");

                image_bind_groups.0.entry(span.image).or_insert_with(|| {
                    render_device.create_bind_group(
                        "pretty_text_texture_bind_group",
                        &material_pipeline.texture_layout,
                        &BindGroupEntries::sequential((&image.texture_view, &image.sampler)),
                    )
                });

                let atlas_extent = image.size_2d().as_vec2();
                let span_color = span.color;

                let batch_start = index;
                for &glyph in span.extracted.iter().map(|i| &extracted_glyphs[*i]) {
                    let glyph_rect = glyph.rect;
                    let rect_size = glyph_rect.size().extend(1.0);

                    let mut i = 0;
                    let positions = QUAD_VERTEX_POSITIONS.map(|pos| {
                        let matrix = glyph.vertices[i] * Mat4::from_scale(rect_size);
                        i += 1;
                        matrix.transform_point3(pos.extend(0f32))
                    });

                    let uvs = [
                        Vec2::new(glyph.rect.min.x, glyph.rect.max.y),
                        Vec2::new(glyph.rect.max.x, glyph.rect.max.y),
                        Vec2::new(glyph.rect.max.x, glyph.rect.min.y),
                        Vec2::new(glyph.rect.min.x, glyph.rect.min.y),
                    ]
                    .map(|pos| pos / atlas_extent);

                    let colors = glyph.colors;

                    for i in QUAD_INDICES {
                        glyph_meta.vertices.push(GlyphVertex {
                            position: positions[i].into(),
                            uv: uvs[i].into(),
                            color: colors[i],
                        });
                    }

                    glyph_meta.instances.push(GlyphInstance {
                        span_color,
                        scale: glyph.glyph_scale,
                        index: glyph.index,
                    });

                    index += 1;
                }

                let i = batches.len() as u32;
                item.batch_range = i..i + 1;
                batches.push((
                    item.entity(),
                    GlyphBatch {
                        range: batch_start..index,
                        material: *span_material,
                        image: span.image,
                    },
                ));
            }
        }

        glyph_meta
            .vertices
            .write_buffer(&render_device, &render_queue);
        glyph_meta
            .instances
            .write_buffer(&render_device, &render_queue);

        *previous_len = batches.len();
        commands.try_insert_batch(batches);
    }
}

fn queue_glyphs<M: GlyphMaterial>(
    extracted_spans: Res<ExtractedGlyphSpans>,
    draw_functions: Res<DrawFunctions<Transparent2d>>,
    material_pipeline: Res<GlyphMaterial2dPipeline<M>>,
    mut pipelines: ResMut<SpecializedRenderPipelines<GlyphMaterial2dPipeline<M>>>,
    pipeline_cache: Res<PipelineCache>,
    render_materials: Res<RenderAssets<PreparedGlyphMaterial2d<M>>>,
    mut transparent_render_phases: ResMut<ViewSortedRenderPhases<Transparent2d>>,
    glyph_meta: Res<GlyphMaterialMeta<M>>,
    mut views: Query<(
        &ExtractedView,
        &Msaa,
        Option<&Tonemapping>,
        Option<&DebandDither>,
    )>,
) where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    let draw_function = draw_functions.read().id::<DrawGlyphMaterial2d<M>>();

    for (view, msaa, tonemapping, dither) in &mut views {
        let Some(transparent_phase) = transparent_render_phases.get_mut(&view.retained_view_entity)
        else {
            continue;
        };

        let msaa_key = SpritePipelineKey::from_msaa_samples(msaa.samples());
        let mut view_key = SpritePipelineKey::from_hdr(view.hdr) | msaa_key;

        if !view.hdr {
            if let Some(tonemapping) = tonemapping {
                view_key |= SpritePipelineKey::TONEMAP_IN_SHADER;
                view_key |= match tonemapping {
                    Tonemapping::None => SpritePipelineKey::TONEMAP_METHOD_NONE,
                    Tonemapping::Reinhard => SpritePipelineKey::TONEMAP_METHOD_REINHARD,
                    Tonemapping::ReinhardLuminance => {
                        SpritePipelineKey::TONEMAP_METHOD_REINHARD_LUMINANCE
                    }
                    Tonemapping::AcesFitted => SpritePipelineKey::TONEMAP_METHOD_ACES_FITTED,
                    Tonemapping::AgX => SpritePipelineKey::TONEMAP_METHOD_AGX,
                    Tonemapping::SomewhatBoringDisplayTransform => {
                        SpritePipelineKey::TONEMAP_METHOD_SOMEWHAT_BORING_DISPLAY_TRANSFORM
                    }
                    Tonemapping::TonyMcMapface => SpritePipelineKey::TONEMAP_METHOD_TONY_MC_MAPFACE,
                    Tonemapping::BlenderFilmic => SpritePipelineKey::TONEMAP_METHOD_BLENDER_FILMIC,
                };
            }
            if let Some(DebandDither::Enabled) = dither {
                view_key |= SpritePipelineKey::DEBAND_DITHER;
            }
        }

        let pipeline = pipelines.specialize(&pipeline_cache, &material_pipeline, view_key);

        if transparent_phase.items.capacity() < extracted_spans.len() {
            transparent_phase
                .items
                .reserve(extracted_spans.len() - transparent_phase.items.capacity());
        }

        for (index, span_material) in glyph_meta.materials.iter() {
            if render_materials.get(*span_material).is_none() {
                continue;
            }

            let extracted_span = &extracted_spans.0[*index];
            match extracted_span.kind {
                ExtractedGlyphSpanKind::Sprite => {
                    transparent_phase.add(Transparent2d {
                        draw_function,
                        pipeline,
                        entity: (extracted_span.render_entity, extracted_span.main_entity),
                        sort_key: FloatOrd(extracted_span.sork_key),
                        batch_range: 0..0,
                        extra_index: PhaseItemExtraIndex::None,
                        extracted_index: *index,
                        indexed: false,
                    });
                }
                ExtractedGlyphSpanKind::Ui { .. } => {
                    continue;
                }
            }
        }
    }
}
