use core::marker::PhantomData;
use std::hash::Hash;

use bevy::ecs::query::ROQueryItem;
use bevy::ecs::system::lifetimeless::{Read, SRes};
use bevy::math::{Affine2, Affine3A, FloatOrd, Mat4, Vec2};
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
use bevy::shader::ShaderRef;
use bevy::text::{ComputedTextBlock, PositionedGlyph};
use bevy::ui_render::{TransparentUi, UiCameraMap, UiCameraView, UiPipelineKey};
use bevy::{ecs::system::*, render::texture::GpuImage};

use crate::effects::material::{DEFAULT_GLYPH_SHADER_HANDLE, GlyphMaterial};
use crate::glyph::{
    Glyph, GlyphIndex, GlyphScale, GlyphVertices, Glyphs, RetainedInheritedVisibility, SpanGlyphOf,
};
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
pub(super) struct GlyphMaterialUiPipeline<M: GlyphMaterial> {
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
    let material_layout = M::bind_group_layout(&render_device);

    commands.insert_resource(GlyphMaterialUiPipeline {
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

impl<M: GlyphMaterial> SpecializedRenderPipeline for GlyphMaterialUiPipeline<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    type Key = UiPipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let buffers = super::vertex_buffer_layouts().to_vec();
        let shader_defs = if key.anti_alias {
            vec!["ANTI_ALIAS".into()]
        } else {
            Vec::new()
        };

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
                    format: if key.hdr {
                        ViewTarget::TEXTURE_FORMAT_HDR
                    } else {
                        TextureFormat::bevy_default()
                    },
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            layout: vec![
                self.view_layout.clone(),
                self.texture_layout.clone(),
                self.material_layout.clone(),
            ],
            label: Some("pretty_text_ui_pipeline".into()),
            ..Default::default()
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
    type Param = SRes<RenderAssets<PreparedGlyphMaterialUi<M>>>;
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
    _marker: PhantomData<T>,
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
        _previous_asset: Option<&Self>,
    ) -> Result<Self, PrepareAssetError<Self::SourceAsset>> {
        match material.as_bind_group(&pipeline.material_layout, render_device, material_param) {
            Ok(prepared) => Ok(PreparedGlyphMaterialUi {
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
    uitext_query: Extract<
        Query<
            (
                Entity,
                &ComputedNode,
                &UiGlobalTransform,
                Option<&CalculatedClip>,
                &ComputedUiTargetCamera,
                &Glyphs,
                &RetainedInheritedVisibility,
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
    camera_map: Extract<UiCameraMap>,
) {
    let mut index = extracted_glyphs.len();
    let mut extracted = Vec::new();

    let mut camera_mapper = camera_map.get_mapper();
    for (entity, uinode, global_transform, clip, camera, glyph_entities, inherited_visibility) in
        &uitext_query
    {
        if !inherited_visibility.0.get() || uinode.is_empty() {
            continue;
        }

        let Some(extracted_camera_entity) = camera_mapper.map(camera) else {
            continue;
        };

        let transform_2d =
            Affine2::from(*global_transform) * Affine2::from_translation(-0.5 * uinode.size());

        // sadge
        let transform = Affine3A::from_mat3_translation(
            Mat3::from_cols(
                transform_2d.matrix2.x_axis.extend(0.0),
                transform_2d.matrix2.y_axis.extend(0.0),
                Vec3::Z,
            ),
            transform_2d.translation.extend(0.0),
        );

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
                        let glyph_transform = v.compute_transform();
                        let corrected_translation = glyph_transform
                            .translation
                            .with_y(-glyph_transform.translation.y);

                        transform
                            * Mat4::from_translation(position.extend(0.))
                            * glyph_transform
                                .with_translation(corrected_translation)
                                .with_rotation(glyph_transform.rotation.normalize().inverse())
                                .compute_affine()
                    }),
                    colors: glyph_vertices.0.map(|v| v.color.to_srgba().to_f32_array()),
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
                    kind: ExtractedGlyphSpanKind::Ui {
                        clip: clip.map(|clip| clip.clip),
                        extracted_camera_entity,
                    },
                    sork_key: uinode.stack_index as f32 + bevy::ui_render::stack_z_offsets::TEXT,
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
    material_pipeline: Res<GlyphMaterialUiPipeline<T>>,
    gpu_images: Res<RenderAssets<GpuImage>>,
    mut phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut previous_len: Local<usize>,
) {
    if view_uniforms.uniforms.binding().is_none() {
        return;
    }

    let mut batches: Vec<(Entity, GlyphBatch<T>)> = Vec::with_capacity(*previous_len);
    let mut index = glyph_meta.instances.len() as u32;

    let glyph_meta = glyph_meta.into_inner();
    for phase in phases.values_mut() {
        for item_index in 0..phase.items.len() {
            let item = &mut phase.items[item_index];

            let Some(span_material) = glyph_meta.materials.get(&item.index) else {
                continue;
            };

            let Some(span) = extracted_spans.0.get(item.index).filter(|span| {
                span.render_entity == item.entity()
                    && matches!(span.kind, ExtractedGlyphSpanKind::Ui { .. })
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

                // Specify the corners of the glyph
                let mut i = 0;
                let positions = QUAD_VERTEX_POSITIONS.map(|pos| {
                    let matrix = glyph.vertices[i] * Mat4::from_scale(rect_size);
                    i += 1;
                    matrix.transform_point3(pos.extend(0f32))
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

                let colors = glyph.colors;

                for i in QUAD_INDICES {
                    glyph_meta.vertices.push(GlyphVertex {
                        position: positions_clipped[i].into(),
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

fn queue_glyphs<M: GlyphMaterial>(
    extracted_spans: Res<ExtractedGlyphSpans>,
    draw_functions_ui: Res<DrawFunctions<TransparentUi>>,
    material_pipeline: Res<GlyphMaterialUiPipeline<M>>,
    mut pipelines: ResMut<SpecializedRenderPipelines<GlyphMaterialUiPipeline<M>>>,
    pipeline_cache: Res<PipelineCache>,
    render_materials: Res<RenderAssets<PreparedGlyphMaterialUi<M>>>,
    mut transparent_render_phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    render_views: Query<(&UiCameraView, Option<&UiAntiAlias>), With<ExtractedView>>,
    camera_views: Query<&ExtractedView>,
    glyph_meta: Res<GlyphMaterialMeta<M>>,
) where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    let draw_function = draw_functions_ui.read().id::<DrawGlyphMaterialUi<M>>();
    let mut current_camera_entity = Entity::PLACEHOLDER;
    let mut current_phase = None;

    for (index, span_material) in glyph_meta.materials.iter() {
        if render_materials.get(*span_material).is_none() {
            continue;
        }

        let extracted_span = &extracted_spans.0[*index];
        match extracted_span.kind {
            ExtractedGlyphSpanKind::Sprite => {
                continue;
            }
            ExtractedGlyphSpanKind::Ui {
                extracted_camera_entity,
                ..
            } => {
                if current_camera_entity != extracted_camera_entity {
                    current_phase = render_views.get(extracted_camera_entity).ok().and_then(
                        |(default_camera_view, ui_anti_alias)| {
                            camera_views
                                .get(default_camera_view.0)
                                .ok()
                                .and_then(|view| {
                                    transparent_render_phases
                                        .get_mut(&view.retained_view_entity)
                                        .map(|transparent_phase| {
                                            (view, ui_anti_alias, transparent_phase)
                                        })
                                })
                        },
                    );
                    current_camera_entity = extracted_camera_entity;
                }

                let Some((view, ui_anti_alias, transparent_phase)) = current_phase.as_mut() else {
                    continue;
                };

                let pipeline = pipelines.specialize(
                    &pipeline_cache,
                    &material_pipeline,
                    UiPipelineKey {
                        hdr: view.hdr,
                        anti_alias: matches!(ui_anti_alias, None | Some(UiAntiAlias::On)),
                    },
                );

                transparent_phase.add(TransparentUi {
                    sort_key: FloatOrd(extracted_span.sork_key),
                    entity: (extracted_span.render_entity, extracted_span.main_entity),
                    pipeline,
                    draw_function,
                    batch_range: 0..0,
                    extra_index: PhaseItemExtraIndex::None,
                    index: *index,
                    indexed: false,
                });
            }
        }
    }
}
