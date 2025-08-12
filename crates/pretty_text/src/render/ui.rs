use core::marker::PhantomData;
use std::hash::Hash;

use bevy::ecs::query::ROQueryItem;
use bevy::ecs::system::lifetimeless::{Read, SRes};
use bevy::math::{FloatOrd, Mat4, Vec2};
use bevy::render::RenderApp;
use bevy::render::globals::GlobalsUniform;
use bevy::render::render_asset::{PrepareAssetError, RenderAsset, RenderAssetPlugin};
use bevy::render::render_resource::binding_types::uniform_buffer;
use bevy::render::sync_world::TemporaryRenderEntity;
use bevy::render::{
    Extract, ExtractSchedule, Render, RenderSet,
    globals::GlobalsBuffer,
    render_asset::RenderAssets,
    render_phase::*,
    render_resource::*,
    renderer::{RenderDevice, RenderQueue},
    view::*,
};
use bevy::text::{ComputedTextBlock, PositionedGlyph};
use bevy::transform::prelude::GlobalTransform;
use bevy::ui::{RenderUiSystem, TransparentUi, UiCameraMap, UiCameraView};
use bevy::{ecs::system::*, render::texture::GpuImage};

use crate::effects::EffectQuery;
use crate::effects::material::{DEFAULT_GLYPH_SHADER_HANDLE, GlyphMaterial, PrettyTextMaterial};
use crate::glyph::{Glyph, GlyphIndex, GlyphScale, GlyphSpan, Glyphs};
use crate::render::{GlyphInstance, GlyphVertex};
use crate::*;

use super::{
    DrawGlyph, ExtractedGlyph, ExtractedGlyphSpan, ExtractedGlyphSpanKind, ExtractedGlyphSpans,
    ExtractedGlyphs, GlyphBatch, GlyphMaterialMeta, ImageBindGroups, MaterialKey,
    SetTextureBindGroup,
};

const QUAD_VERTEX_POSITIONS: [Vec2; 4] = [
    Vec2::new(-0.5, -0.5),
    Vec2::new(0.5, -0.5),
    Vec2::new(0.5, 0.5),
    Vec2::new(-0.5, 0.5),
];

const QUAD_INDICES: [usize; 6] = [0, 2, 3, 0, 1, 2];

pub(super) struct GlyphMaterialUiPlugin<T: GlyphMaterial>(PhantomData<T>);

impl<T: GlyphMaterial> Default for GlyphMaterialUiPlugin<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: GlyphMaterial> Plugin for GlyphMaterialUiPlugin<T>
where
    T::Data: PartialEq + Eq + Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.init_asset::<T>()
            .add_plugins(RenderAssetPlugin::<PreparedGlyphMaterialUi<T>>::default());

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_render_command::<TransparentUi, DrawGlyphMaterialUi<T>>()
                .init_resource::<SpecializedRenderPipelines<GlyphMaterialUiPipeline<T>>>()
                .add_systems(
                    ExtractSchedule,
                    extract_glyphs::<T>.in_set(RenderUiSystem::ExtractText),
                )
                .add_systems(
                    Render,
                    (
                        (
                            prepare_view_bind_groups::<T>,
                            prepare_glyphs::<T>.after(super::prepare_glyph_meta::<T>),
                        )
                            .chain()
                            .in_set(RenderSet::PrepareBindGroups),
                        queue_glyphs::<T>.in_set(RenderSet::Queue),
                    ),
                );
        }
    }

    fn finish(&self, app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app.init_resource::<GlyphMaterialUiPipeline<T>>();
        }
    }
}

#[derive(Resource)]
pub(super) struct GlyphMaterialUiPipeline<M: GlyphMaterial> {
    view_layout: BindGroupLayout,
    texture_layout: BindGroupLayout,
    material_layout: BindGroupLayout,
    vertex_shader: Option<Handle<Shader>>,
    fragment_shader: Option<Handle<Shader>>,
    marker: PhantomData<M>,
}

impl<M: GlyphMaterial> SpecializedRenderPipeline for GlyphMaterialUiPipeline<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    type Key = MaterialKey<M>;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let buffers = super::vertex_buffer_layouts().to_vec();
        let shader_defs = Vec::new();

        let mut descriptor = RenderPipelineDescriptor {
            vertex: VertexState {
                shader: DEFAULT_GLYPH_SHADER_HANDLE,
                entry_point: "vertex".into(),
                shader_defs: shader_defs.clone(),
                buffers,
            },
            fragment: Some(FragmentState {
                shader: DEFAULT_GLYPH_SHADER_HANDLE,
                shader_defs,
                entry_point: "fragment".into(),
                targets: vec![Some(ColorTargetState {
                    format: if key.hdr {
                        ViewTarget::TEXTURE_FORMAT_HDR
                    } else {
                        TextureFormat::bevy_default()
                    },
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            layout: vec![],
            push_constant_ranges: Vec::new(),
            primitive: PrimitiveState {
                front_face: FrontFace::Ccw,
                cull_mode: None,
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
            },
            depth_stencil: None,
            multisample: MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            label: Some("pretty_text_ui_pipeline".into()),
            zero_initialize_workgroup_memory: false,
        };
        if let Some(vertex_shader) = &self.vertex_shader {
            descriptor.vertex.shader = vertex_shader.clone();
        }

        if let Some(fragment_shader) = &self.fragment_shader {
            descriptor.fragment.as_mut().unwrap().shader = fragment_shader.clone();
        }

        descriptor.layout = vec![
            self.view_layout.clone(),
            self.texture_layout.clone(),
            self.material_layout.clone(),
        ];

        descriptor
    }
}

impl<M: GlyphMaterial> FromWorld for GlyphMaterialUiPipeline<M> {
    fn from_world(world: &mut World) -> Self {
        let asset_server = world.resource::<AssetServer>();
        let render_device = world.resource::<RenderDevice>();

        let view_layout = render_device.create_bind_group_layout(
            "pretty_text_ui_view_layout",
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
        let material_layout = M::bind_group_layout(render_device);

        GlyphMaterialUiPipeline {
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
            marker: PhantomData,
        }
    }
}

pub(super) type DrawGlyphMaterialUi<M> = (
    SetItemPipeline,
    SetViewBindGroup<M, 0>,
    SetTextureBindGroup<M, 1>,
    SetMaterialBindGroup<M, 2>,
    DrawGlyph<M>,
);

pub(super) struct SetViewBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P> for SetViewBindGroup<M, I> {
    type Param = ();
    type ViewQuery = (Read<ViewUniformOffset>, Read<GlyphUiViewBindGroup>);
    type ItemQuery = ();

    fn render<'w>(
        _item: &P,
        (view_uniform, bind_group): ROQueryItem<'w, Self::ViewQuery>,
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
    type Param = SRes<RenderAssets<PreparedGlyphMaterialUi<M>>>;
    type ViewQuery = ();
    type ItemQuery = Read<GlyphBatch<M>>;

    fn render<'w>(
        _item: &P,
        _view: (),
        material_handle: Option<ROQueryItem<'_, Self::ItemQuery>>,
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
pub(super) struct GlyphUiViewBindGroup(BindGroup);

fn prepare_view_bind_groups<T: GlyphMaterial>(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    pipeline: Res<GlyphMaterialUiPipeline<T>>,
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
            "pretty_text_ui_view_bind_group",
            &pipeline.view_layout,
            &BindGroupEntries::sequential((view_binding.clone(), globals_binding.clone())),
        );

        commands
            .entity(entity)
            .insert(GlyphUiViewBindGroup(view_bind_group));
    }
}

pub(super) struct PreparedGlyphMaterialUi<T: GlyphMaterial> {
    _bindings: BindingResources,
    bind_group: BindGroup,
    key: T::Data,
}

impl<M: GlyphMaterial> RenderAsset for PreparedGlyphMaterialUi<M> {
    type SourceAsset = M;

    type Param = (
        SRes<RenderDevice>,
        SRes<GlyphMaterialUiPipeline<M>>,
        M::Param,
    );

    fn prepare_asset(
        material: Self::SourceAsset,
        _: AssetId<Self::SourceAsset>,
        (render_device, pipeline, material_param): &mut SystemParamItem<Self::Param>,
    ) -> Result<Self, PrepareAssetError<Self::SourceAsset>> {
        match material.as_bind_group(&pipeline.material_layout, render_device, material_param) {
            Ok(prepared) => Ok(PreparedGlyphMaterialUi {
                _bindings: prepared.bindings,
                bind_group: prepared.bind_group,
                key: prepared.data,
            }),
            Err(AsBindGroupError::RetryNextUpdate) => {
                Err(PrepareAssetError::RetryNextUpdate(material))
            }
            Err(other) => Err(PrepareAssetError::AsBindGroupError(other)),
        }
    }
}

// Extract the glyph spans that are rendered with `T`.
fn extract_glyphs<T: GlyphMaterial>(
    mut commands: Commands,
    mut extracted_spans: ResMut<ExtractedGlyphSpans<T>>,
    mut extracted_glyphs: ResMut<ExtractedGlyphs>,
    texture_atlases: Extract<Res<Assets<TextureAtlasLayout>>>,
    uitext_query: Extract<
        Query<
            (
                Entity,
                &ComputedNode,
                &GlobalTransform,
                Option<&CalculatedClip>,
                &ComputedNodeTarget,
                &Glyphs,
            ),
            With<ComputedTextBlock>,
        >,
    >,
    glyphs: Extract<
        Query<(
            &Glyph,
            &GlyphSpan,
            &InheritedVisibility,
            &Transform,
            &GlyphScale,
            &GlyphIndex,
        )>,
    >,
    text_styles: Extract<Query<&TextColor>>,
    text_materials: Extract<EffectQuery<&PrettyTextMaterial<T>>>,
    camera_map: Extract<UiCameraMap>,
) {
    let mut index = extracted_glyphs.len();
    let mut extracted = Vec::new();

    let mut camera_mapper = camera_map.get_mapper();
    for (entity, uinode, global_transform, clip, camera, glyph_entities) in &uitext_query {
        // glyph entities are the source of truth for visibility
        if uinode.is_empty() {
            continue;
        }

        let Some(extracted_camera_entity) = camera_mapper.map(camera) else {
            continue;
        };

        let transform = global_transform.affine()
            * bevy::math::Affine3A::from_translation((-0.5 * uinode.size()).extend(0.));

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
            glyph_transform,
            glyph_scale,
            glyph_index,
        )) = iter.next()
        {
            if inherited_visibility.get() && text_materials.get(span_entity.0).is_ok() {
                let rect = texture_atlases
                    .get(&atlas_info.texture_atlas)
                    .unwrap()
                    .textures[atlas_info.location.glyph_index]
                    .as_rect();
                extracted_glyphs.push(ExtractedGlyph {
                    transform: transform
                        * Mat4::from_translation(position.extend(0.))
                        * glyph_transform.compute_affine(),
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
                let material_handle = text_materials.iter(span_entity.0).next().unwrap();
                let color = text_styles
                    .get(span_entity.0)
                    .map(|text_color| LinearRgba::from(text_color.0))
                    .unwrap_or_default();

                extracted_spans.push(ExtractedGlyphSpan {
                    kind: ExtractedGlyphSpanKind::Ui {
                        clip: clip.map(|clip| clip.clip),
                        extracted_camera_entity,
                    },
                    sork_key: uinode.stack_index as f32 + bevy::ui::stack_z_offsets::MATERIAL,
                    main_entity: entity.into(),
                    render_entity: commands.spawn(TemporaryRenderEntity).id(),
                    color: color.to_f32_array(),
                    image: atlas_info.texture.id(),
                    material: material_handle.0.id(),
                    extracted: extracted.drain(..).collect(),
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
    mut glyph_meta: ResMut<GlyphMaterialMeta<T>>,
    extracted_spans: ResMut<ExtractedGlyphSpans<T>>,
    extracted_glyphs: ResMut<ExtractedGlyphs>,
    view_uniforms: Res<ViewUniforms>,
    material_pipeline: Res<GlyphMaterialUiPipeline<T>>,
    gpu_images: Res<RenderAssets<GpuImage>>,
    mut phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut previous_len: Local<usize>,
) {
    if view_uniforms.uniforms.binding().is_none() {
        return;
    }

    let mut batches: Vec<(Entity, GlyphBatch<T>)> = Vec::with_capacity(*previous_len);
    let mut index = 0;

    for ui_phase in phases.values_mut() {
        let mut batch_item_index = 0;
        let mut batch_shader_handle = AssetId::invalid();
        let mut image_handle = AssetId::invalid();

        for item_index in 0..ui_phase.items.len() {
            let item = &mut ui_phase.items[item_index];
            if let Some(span) = extracted_spans.get(item.index).filter(|n| {
                item.entity() == n.render_entity
                    && matches!(n.kind, ExtractedGlyphSpanKind::Ui { .. })
            }) {
                let image = gpu_images
                    .get(span.image)
                    .expect("Image was checked during batching and should still exist");

                let mut existing_batch = batches
                    .last_mut()
                    .filter(|_| batch_shader_handle == span.material && image_handle == span.image);

                if existing_batch.is_none() {
                    batch_item_index = item_index;
                    batch_shader_handle = span.material;
                    image_handle = span.image;

                    image_bind_groups.0.entry(span.image).or_insert_with(|| {
                        render_device.create_bind_group(
                            "pretty_text_texture_bind_group",
                            &material_pipeline.texture_layout,
                            &BindGroupEntries::sequential((&image.texture_view, &image.sampler)),
                        )
                    });

                    let new_batch = GlyphBatch {
                        range: index..index,
                        material: span.material,
                        image: span.image,
                    };

                    batches.push((item.entity(), new_batch));

                    existing_batch = batches.last_mut();
                }

                let atlas_extent = image.size_2d().as_vec2();
                let color = span.color;

                for &glyph in span.extracted.iter().map(|i| &extracted_glyphs[*i]) {
                    let glyph_rect = glyph.rect;
                    let rect_size = glyph_rect.size().extend(1.0);

                    // Specify the corners of the glyph
                    let positions = QUAD_VERTEX_POSITIONS.map(|pos| {
                        (glyph.transform * (pos.extend(0.) * rect_size).extend(1.)).xyz()
                    });

                    let ExtractedGlyphSpanKind::Ui { clip, .. } = span.kind else {
                        unreachable!()
                    };
                    let positions_diff = if let Some(clip) = clip {
                        [
                            Vec2::new(
                                f32::max(clip.min.x - positions[0].x, 0.),
                                f32::max(clip.min.y - positions[0].y, 0.),
                            ),
                            Vec2::new(
                                f32::min(clip.max.x - positions[1].x, 0.),
                                f32::max(clip.min.y - positions[1].y, 0.),
                            ),
                            Vec2::new(
                                f32::min(clip.max.x - positions[2].x, 0.),
                                f32::min(clip.max.y - positions[2].y, 0.),
                            ),
                            Vec2::new(
                                f32::max(clip.min.x - positions[3].x, 0.),
                                f32::min(clip.max.y - positions[3].y, 0.),
                            ),
                        ]
                    } else {
                        [Vec2::ZERO; 4]
                    };

                    let positions_clipped = [
                        positions[0] + positions_diff[0].extend(0.),
                        positions[1] + positions_diff[1].extend(0.),
                        positions[2] + positions_diff[2].extend(0.),
                        positions[3] + positions_diff[3].extend(0.),
                    ];

                    let transformed_rect_size = glyph.transform.transform_vector3(rect_size);

                    // Don't try to cull nodes that have a rotation
                    // In a rotation around the Z-axis, this value is 0.0 for an angle of 0.0 or π
                    // In those two cases, the culling check can proceed normally as corners will be on
                    // horizontal / vertical lines
                    // For all other angles, bypass the culling check
                    // This does not properly handles all rotations on all axis
                    if glyph.transform.x_axis[1] == 0.0 {
                        // Cull nodes that are completely clipped
                        if positions_diff[0].x - positions_diff[1].x >= transformed_rect_size.x
                            || positions_diff[1].y - positions_diff[2].y >= transformed_rect_size.y
                        {
                            continue;
                        }
                    }

                    let uvs = [
                        Vec2::new(
                            glyph.rect.min.x + positions_diff[0].x,
                            glyph.rect.min.y + positions_diff[0].y,
                        ),
                        Vec2::new(
                            glyph.rect.max.x + positions_diff[1].x,
                            glyph.rect.min.y + positions_diff[1].y,
                        ),
                        Vec2::new(
                            glyph.rect.max.x + positions_diff[2].x,
                            glyph.rect.max.y + positions_diff[2].y,
                        ),
                        Vec2::new(
                            glyph.rect.min.x + positions_diff[3].x,
                            glyph.rect.max.y + positions_diff[3].y,
                        ),
                    ]
                    .map(|pos| pos / atlas_extent);

                    for i in QUAD_INDICES {
                        glyph_meta.vertices.push(GlyphVertex {
                            position: positions_clipped[i].into(),
                            uv: uvs[i].into(),
                        });
                    }

                    glyph_meta.instances.push(GlyphInstance {
                        color,
                        scale: glyph.glyph_scale.to_array(),
                        index: glyph.index,
                    });

                    index += 1;
                    match &mut existing_batch {
                        Some(existing_batch) => existing_batch.1.range.end = index,
                        None => unreachable!(),
                    }
                }
                ui_phase.items[batch_item_index].batch_range_mut().end += 1;
            } else {
                batch_shader_handle = AssetId::invalid();
                image_handle = AssetId::invalid();
            }
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

fn queue_glyphs<M: GlyphMaterial>(
    extracted_spans: Res<ExtractedGlyphSpans<M>>,
    draw_functions_ui: Res<DrawFunctions<TransparentUi>>,
    material_pipeline: Res<GlyphMaterialUiPipeline<M>>,
    mut pipelines: ResMut<SpecializedRenderPipelines<GlyphMaterialUiPipeline<M>>>,
    pipeline_cache: Res<PipelineCache>,
    render_materials: Res<RenderAssets<PreparedGlyphMaterialUi<M>>>,
    mut transparent_render_phases_ui: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut render_views: Query<&UiCameraView, With<ExtractedView>>,
    camera_views: Query<&ExtractedView>,
) where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    let draw_function = draw_functions_ui.read().id::<DrawGlyphMaterialUi<M>>();

    for (index, extracted_span) in extracted_spans.iter().enumerate() {
        let Some(material) = render_materials.get(extracted_span.material) else {
            continue;
        };

        match extracted_span.kind {
            ExtractedGlyphSpanKind::Sprite => {
                continue;
            }
            ExtractedGlyphSpanKind::Ui {
                extracted_camera_entity,
                ..
            } => {
                let Ok(default_camera_view) = render_views.get_mut(extracted_camera_entity) else {
                    continue;
                };

                let Ok(view) = camera_views.get(default_camera_view.0) else {
                    continue;
                };

                let Some(transparent_phase) =
                    transparent_render_phases_ui.get_mut(&view.retained_view_entity)
                else {
                    continue;
                };

                let pipeline = pipelines.specialize(
                    &pipeline_cache,
                    &material_pipeline,
                    MaterialKey {
                        hdr: view.hdr,
                        bind_group_data: material.key.clone(),
                    },
                );

                if transparent_phase.items.capacity() < extracted_spans.len() {
                    transparent_phase
                        .items
                        .reserve(extracted_spans.len() - transparent_phase.items.capacity());
                }

                transparent_phase.add(TransparentUi {
                    draw_function,
                    pipeline,
                    entity: (extracted_span.render_entity, extracted_span.main_entity),
                    sort_key: FloatOrd(extracted_span.sork_key),
                    batch_range: 0..0,
                    extra_index: PhaseItemExtraIndex::None,
                    index,
                    indexed: false,
                });
            }
        }
    }
}
