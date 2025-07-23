//! Custom render pipeline for rendering [`TextGlyph`](crate::glyph::TextGlyph)s in
//! [`Text`] hierarchies.

use core::{hash::Hash, marker::PhantomData, ops::Range};

use crate::glyph::{Glyph, GlyphOffset, GlyphSpanEntity, Glyphs};
use crate::material::{DEFAULT_GLYPH_SHADER_HANDLE, GlyphMaterial};
use crate::*;
use bevy::math::{FloatOrd, Mat4, Rect, Vec2};
use bevy::render::RenderApp;
use bevy::render::extract_component::ExtractComponent;
use bevy::render::sync_world::{MainEntity, TemporaryRenderEntity};
use bevy::render::{
    Extract, ExtractSchedule, Render, RenderSet,
    extract_component::ExtractComponentPlugin,
    globals::{GlobalsBuffer, GlobalsUniform},
    render_asset::{PrepareAssetError, RenderAsset, RenderAssetPlugin, RenderAssets},
    render_phase::*,
    render_resource::{binding_types::uniform_buffer, *},
    renderer::{RenderDevice, RenderQueue},
    view::*,
};
use bevy::text::{ComputedTextBlock, PositionedGlyph, TextLayoutInfo};
use bevy::transform::prelude::GlobalTransform;
use bevy::ui::{RenderUiSystem, TransparentUi, UiCameraMap, UiCameraView, extract_text_sections};
use bevy::{
    ecs::{
        prelude::Component,
        query::ROQueryItem,
        system::{
            lifetimeless::{Read, SRes},
            *,
        },
    },
    render::texture::GpuImage,
};

/// A [material](GlyphMaterial) for rendering [`TextGlyph`](crate::glyph::TextGlyph)s.
///
/// [`TextGlyphMaterial`] is inserted into text spans in a [`Text`] hierarchy.
/// [`Glyph`]s are batched together and rendered with one instance of the material.
#[derive(Debug, Clone, Component, ExtractComponent, Reflect)]
pub struct TextGlyphMaterial<T: GlyphMaterial>(pub Handle<T>);

/// Adds the necessary ECS resources and render logic to enable rendering entities
/// using the given [`GlyphMaterial`] asset type.
#[derive(Debug)]
pub struct GlyphMaterialPlugin<M: GlyphMaterial>(PhantomData<M>);

impl<M: GlyphMaterial> Default for GlyphMaterialPlugin<M> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<M: GlyphMaterial> Plugin for GlyphMaterialPlugin<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.init_asset::<M>()
            .register_type::<TextGlyphMaterial<M>>()
            .add_plugins((
                ExtractComponentPlugin::<TextGlyphMaterial<M>>::extract_visible(),
                RenderAssetPlugin::<PreparedGlyphMaterial<M>>::default(),
            ));

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_render_command::<TransparentUi, DrawGlyphMaterial<M>>()
                .init_resource::<ExtractedGlyphSpans<M>>()
                .init_resource::<ExtractedGlyphs>()
                .init_resource::<GlyphMaterialMeta<M>>()
                .init_resource::<SpecializedRenderPipelines<GlyphMaterialPipeline<M>>>()
                .add_systems(
                    ExtractSchedule,
                    extract_glyphs::<M>
                        .in_set(RenderUiSystem::ExtractText)
                        .before(extract_text_sections),
                )
                .add_systems(
                    Render,
                    (
                        queue_glyphs::<M>.in_set(RenderSet::Queue),
                        prepare_glyphs::<M>.in_set(RenderSet::PrepareBindGroups),
                        clear_glyphs.after(RenderSet::PrepareBindGroups),
                    ),
                );
        }
    }

    fn finish(&self, app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app.init_resource::<GlyphMaterialPipeline<M>>();
        }
    }
}

#[derive(Resource)]
struct GlyphMaterialMeta<M: GlyphMaterial> {
    vertices: RawBufferVec<GlyphVertex>,
    view_bind_group: Option<BindGroup>,
    marker: PhantomData<M>,
}

impl<M: GlyphMaterial> Default for GlyphMaterialMeta<M> {
    fn default() -> Self {
        Self {
            vertices: RawBufferVec::new(BufferUsages::VERTEX),
            view_bind_group: Default::default(),
            marker: PhantomData,
        }
    }
}

#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct GlyphVertex {
    position: [f32; 3],
    uv: [f32; 2],
    color: [f32; 4],
}

#[derive(Component)]
struct GlyphBatch<M: GlyphMaterial> {
    /// The range of vertices inside the [`GlyphMaterialMeta`].
    range: Range<u32>,
    material: AssetId<M>,
}

/// Render pipeline data for a given [`GlyphMaterial`].
#[derive(Resource)]
struct GlyphMaterialPipeline<M: GlyphMaterial> {
    ui_layout: BindGroupLayout,
    view_layout: BindGroupLayout,
    vertex_shader: Option<Handle<Shader>>,
    fragment_shader: Option<Handle<Shader>>,
    marker: PhantomData<M>,
}

impl<M: GlyphMaterial> SpecializedRenderPipeline for GlyphMaterialPipeline<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    type Key = UiMaterialKey<M>;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        // Custom layout to match the mesh2d layout
        let vertex_layout = VertexBufferLayout {
            array_stride: VertexFormat::Float32x3.size()
                + VertexFormat::Float32x2.size()
                + VertexFormat::Float32x4.size(),
            step_mode: VertexStepMode::Vertex,
            attributes: vec![
                VertexAttribute {
                    format: VertexFormat::Float32x3,
                    offset: 0,
                    shader_location: 0,
                },
                VertexAttribute {
                    format: VertexFormat::Float32x2,
                    offset: VertexFormat::Float32x3.size(),
                    shader_location: 2,
                },
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: VertexFormat::Float32x3.size() + VertexFormat::Float32x2.size(),
                    shader_location: 4,
                },
            ],
        };
        let shader_defs = vec!["TEXT_UI".into()];

        let mut descriptor = RenderPipelineDescriptor {
            vertex: VertexState {
                shader: DEFAULT_GLYPH_SHADER_HANDLE,
                entry_point: "vertex".into(),
                shader_defs: shader_defs.clone(),
                buffers: vec![vertex_layout],
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
            label: Some("pretty_text_glyph_pipeline".into()),
            zero_initialize_workgroup_memory: false,
        };
        if let Some(vertex_shader) = &self.vertex_shader {
            descriptor.vertex.shader = vertex_shader.clone();
        }

        if let Some(fragment_shader) = &self.fragment_shader {
            descriptor.fragment.as_mut().unwrap().shader = fragment_shader.clone();
        }

        descriptor.layout = vec![self.view_layout.clone(), self.ui_layout.clone()];

        <M as UiMaterial>::specialize(&mut descriptor, key);

        descriptor
    }
}

impl<M: GlyphMaterial> FromWorld for GlyphMaterialPipeline<M> {
    fn from_world(world: &mut World) -> Self {
        let asset_server = world.resource::<AssetServer>();
        let render_device = world.resource::<RenderDevice>();
        let ui_layout = M::bind_group_layout(render_device);

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

        GlyphMaterialPipeline {
            ui_layout,
            view_layout,
            vertex_shader: match <M as UiMaterial>::vertex_shader() {
                ShaderRef::Default => None,
                ShaderRef::Handle(handle) => Some(handle),
                ShaderRef::Path(path) => Some(asset_server.load(path)),
            },
            fragment_shader: match <M as UiMaterial>::fragment_shader() {
                ShaderRef::Default => None,
                ShaderRef::Handle(handle) => Some(handle),
                ShaderRef::Path(path) => Some(asset_server.load(path)),
            },
            marker: PhantomData,
        }
    }
}

type DrawGlyphMaterial<M> = (
    SetItemPipeline,
    SetMatUiViewBindGroup<M, 0>,
    SetUiMaterialBindGroup<M, 1>,
    DrawGlyph<M>,
);

struct SetMatUiViewBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P>
    for SetMatUiViewBindGroup<M, I>
{
    type Param = SRes<GlyphMaterialMeta<M>>;
    type ViewQuery = Read<ViewUniformOffset>;
    type ItemQuery = ();

    fn render<'w>(
        _item: &P,
        view_uniform: &'w ViewUniformOffset,
        _entity: Option<()>,
        ui_meta: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        pass.set_bind_group(
            I,
            ui_meta.into_inner().view_bind_group.as_ref().unwrap(),
            &[view_uniform.offset],
        );
        RenderCommandResult::Success
    }
}

struct SetUiMaterialBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P>
    for SetUiMaterialBindGroup<M, I>
{
    type Param = SRes<RenderAssets<PreparedGlyphMaterial<M>>>;
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

struct DrawGlyph<M>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial> RenderCommand<P> for DrawGlyph<M> {
    type Param = SRes<GlyphMaterialMeta<M>>;
    type ViewQuery = ();
    type ItemQuery = Read<GlyphBatch<M>>;

    #[inline]
    fn render<'w>(
        _item: &P,
        _view: (),
        batch: Option<&'w GlyphBatch<M>>,
        ui_meta: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(batch) = batch else {
            return RenderCommandResult::Skip;
        };

        pass.set_vertex_buffer(0, ui_meta.into_inner().vertices.buffer().unwrap().slice(..));
        pass.draw(batch.range.clone(), 0..1);
        RenderCommandResult::Success
    }
}

/// A span of `ExtractedGlyph` instances.
///
/// This represents a `GlyphSpanEntity`.
struct ExtractedGlyphSpan<M: GlyphMaterial> {
    // root components
    stack_index: u32,
    clip: Option<Rect>,
    // Camera to render this UI node to. By the time it is extracted,
    // it is defaulted to a single camera if only one exists.
    // Nodes with ambiguous camera will be ignored.
    extracted_camera_entity: Entity,

    // span components
    main_entity: MainEntity,
    render_entity: Entity,
    color: [f32; 4],
    image: AssetId<Image>,
    material: AssetId<M>,
    extracted: Vec<usize>,
}

/// Spans of `ExtractedGlyph` instances.
#[derive(Deref, DerefMut, Resource)]
struct ExtractedGlyphSpans<M: GlyphMaterial>(Vec<ExtractedGlyphSpan<M>>);

impl<M: GlyphMaterial> Default for ExtractedGlyphSpans<M> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

/// Represents a `Glyph`.
#[derive(Clone, Copy)]
struct ExtractedGlyph {
    transform: Mat4,
    rect: Rect,
}

/// Collection of `Glyph`s.
#[derive(Default, Deref, DerefMut, Resource)]
struct ExtractedGlyphs(Vec<ExtractedGlyph>);

// Extract the glyph spans that are rendered with `M`.
fn extract_glyphs<M: GlyphMaterial>(
    mut commands: Commands,
    mut extracted_spans: ResMut<ExtractedGlyphSpans<M>>,
    mut extracted_glyphs: ResMut<ExtractedGlyphs>,
    texture_atlases: Extract<Res<Assets<TextureAtlasLayout>>>,
    uitext_query: Extract<
        Query<(
            Entity,
            &ComputedNode,
            &GlobalTransform,
            Option<&CalculatedClip>,
            &ComputedNodeTarget,
            &ComputedTextBlock,
            &TextLayoutInfo,
            &Glyphs,
        )>,
    >,
    glyphs: Extract<Query<(&Glyph, &GlyphSpanEntity, &InheritedVisibility, &GlyphOffset)>>,
    text_styles: Extract<Query<&TextColor>>,
    text_materials: Extract<Query<&TextGlyphMaterial<M>>>,
    camera_map: Extract<UiCameraMap>,
) {
    let mut index = extracted_glyphs.len();
    let mut extracted = Vec::new();

    let mut camera_mapper = camera_map.get_mapper();
    for (
        entity,
        uinode,
        global_transform,
        clip,
        camera,
        computed_block,
        text_layout_info,
        glyph_entities,
    ) in &uitext_query
    {
        // glyph entities are the source of truth for visibility
        if uinode.is_empty() {
            continue;
        }

        let Some(extracted_camera_entity) = camera_mapper.map(camera) else {
            continue;
        };

        let transform = global_transform.affine()
            * bevy::math::Affine3A::from_translation((-0.5 * uinode.size()).extend(0.));

        for (
            i,
            (
                Glyph(PositionedGlyph {
                    position,
                    atlas_info,
                    span_index,
                    ..
                }),
                span_entity,
                inherited_visibility,
                glyph_offset,
            ),
        ) in glyphs.iter_many(glyph_entities.iter()).enumerate()
        {
            if inherited_visibility.get() && text_materials.get(span_entity.0).is_ok() {
                let rect = texture_atlases
                    .get(&atlas_info.texture_atlas)
                    .unwrap()
                    .textures[atlas_info.location.glyph_index]
                    .as_rect();
                extracted_glyphs.push(ExtractedGlyph {
                    transform: transform
                        * Mat4::from_translation((position).extend(0.) + glyph_offset.0),
                    rect,
                });
                extracted.push(index);
                index += 1;
            }

            if !extracted.is_empty()
                && text_layout_info.glyphs.get(i + 1).is_none_or(|info| {
                    info.span_index != *span_index || info.atlas_info.texture != atlas_info.texture
                })
            {
                let color = text_styles
                    .get(
                        computed_block
                            .entities()
                            .get(*span_index)
                            .map(|t| t.entity)
                            .unwrap_or(Entity::PLACEHOLDER),
                    )
                    .map(|text_color| LinearRgba::from(text_color.0))
                    .unwrap_or_default();
                let material_handle = text_materials.get(span_entity.0).unwrap();

                extracted_spans.push(ExtractedGlyphSpan {
                    stack_index: uinode.stack_index,
                    clip: clip.map(|clip| clip.clip),
                    //
                    extracted_camera_entity,
                    main_entity: entity.into(),
                    render_entity: commands.spawn(TemporaryRenderEntity).id(),
                    //
                    color: color.to_f32_array(),
                    image: atlas_info.texture.id(),
                    material: material_handle.0.id(),
                    extracted: std::mem::take(&mut extracted),
                });
            }
        }
    }
}

fn prepare_glyphs<M: GlyphMaterial>(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    mut glyph_meta: ResMut<GlyphMaterialMeta<M>>,
    mut extracted_spans: ResMut<ExtractedGlyphSpans<M>>,
    extracted_glyphs: ResMut<ExtractedGlyphs>,
    view_uniforms: Res<ViewUniforms>,
    globals_buffer: Res<GlobalsBuffer>,
    ui_material_pipeline: Res<GlyphMaterialPipeline<M>>,
    gpu_images: Res<RenderAssets<GpuImage>>,
    mut phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut previous_len: Local<usize>,
) {
    const QUAD_VERTEX_POSITIONS: [Vec2; 4] = [
        Vec2::new(-0.5, -0.5),
        Vec2::new(0.5, -0.5),
        Vec2::new(0.5, 0.5),
        Vec2::new(-0.5, 0.5),
    ];
    const QUAD_INDICES: [usize; 6] = [0, 2, 3, 0, 1, 2];

    if let (Some(view_binding), Some(globals_binding)) = (
        view_uniforms.uniforms.binding(),
        globals_buffer.buffer.binding(),
    ) {
        let mut batches: Vec<(Entity, GlyphBatch<M>)> = Vec::with_capacity(*previous_len);

        glyph_meta.vertices.clear();
        glyph_meta.view_bind_group = Some(render_device.create_bind_group(
            "pretty_text_glyph_view_bind_group",
            &ui_material_pipeline.view_layout,
            &BindGroupEntries::sequential((view_binding, globals_binding)),
        ));
        let mut index = 0;

        for ui_phase in phases.values_mut() {
            let mut batch_item_index = 0;
            let mut batch_shader_handle = AssetId::invalid();

            for item_index in 0..ui_phase.items.len() {
                let item = &mut ui_phase.items[item_index];
                if let Some(span) = extracted_spans
                    .get(item.index)
                    .filter(|n| item.entity() == n.render_entity)
                {
                    let mut existing_batch = batches
                        .last_mut()
                        .filter(|_| batch_shader_handle == span.material);

                    if existing_batch.is_none() {
                        batch_item_index = item_index;
                        batch_shader_handle = span.material;

                        let new_batch = GlyphBatch {
                            range: index..index,
                            material: span.material,
                        };

                        batches.push((item.entity(), new_batch));

                        existing_batch = batches.last_mut();
                    }

                    let image = gpu_images
                        .get(span.image)
                        .expect("Image was checked during batching and should still exist");
                    let atlas_extent = image.size_2d().as_vec2();
                    let color = span.color;

                    for &glyph in span.extracted.iter().map(|i| &extracted_glyphs[*i]) {
                        let glyph_rect = glyph.rect;
                        let rect_size = glyph_rect.size().extend(1.0);

                        // Specify the corners of the glyph
                        let positions = QUAD_VERTEX_POSITIONS.map(|pos| {
                            (glyph.transform * (pos.extend(0.) * rect_size).extend(1.)).xyz()
                        });

                        let positions_diff = if let Some(clip) = span.clip {
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
                                || positions_diff[1].y - positions_diff[2].y
                                    >= transformed_rect_size.y
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
                                color,
                            });
                        }

                        index += QUAD_INDICES.len() as u32;
                        match &mut existing_batch {
                            Some(existing_batch) => existing_batch.1.range.end = index,
                            None => unreachable!(),
                        }
                    }
                    ui_phase.items[batch_item_index].batch_range_mut().end += 1;
                } else {
                    batch_shader_handle = AssetId::invalid();
                }
            }
        }

        glyph_meta
            .vertices
            .write_buffer(&render_device, &render_queue);

        *previous_len = batches.len();
        commands.try_insert_batch(batches);
    }

    extracted_spans.clear();
}

fn clear_glyphs(mut extracted_glyphs: ResMut<ExtractedGlyphs>) {
    extracted_glyphs.clear();
}

struct PreparedGlyphMaterial<T: GlyphMaterial> {
    _bindings: BindingResources,
    bind_group: BindGroup,
    key: T::Data,
}

impl<M: GlyphMaterial> RenderAsset for PreparedGlyphMaterial<M> {
    type SourceAsset = M;

    type Param = (SRes<RenderDevice>, SRes<GlyphMaterialPipeline<M>>, M::Param);

    fn prepare_asset(
        material: Self::SourceAsset,
        _: AssetId<Self::SourceAsset>,
        (render_device, pipeline, material_param): &mut SystemParamItem<Self::Param>,
    ) -> Result<Self, PrepareAssetError<Self::SourceAsset>> {
        match material.as_bind_group(&pipeline.ui_layout, render_device, material_param) {
            Ok(prepared) => Ok(PreparedGlyphMaterial {
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

fn queue_glyphs<M: GlyphMaterial>(
    extracted_spans: Res<ExtractedGlyphSpans<M>>,
    draw_functions: Res<DrawFunctions<TransparentUi>>,
    ui_material_pipeline: Res<GlyphMaterialPipeline<M>>,
    mut pipelines: ResMut<SpecializedRenderPipelines<GlyphMaterialPipeline<M>>>,
    pipeline_cache: Res<PipelineCache>,
    render_materials: Res<RenderAssets<PreparedGlyphMaterial<M>>>,
    mut transparent_render_phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut render_views: Query<&UiCameraView, With<ExtractedView>>,
    camera_views: Query<&ExtractedView>,
) where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    let draw_function = draw_functions.read().id::<DrawGlyphMaterial<M>>();

    for (index, extracted_span) in extracted_spans.iter().enumerate() {
        let Some(material) = render_materials.get(extracted_span.material) else {
            continue;
        };

        let Ok(default_camera_view) = render_views.get_mut(extracted_span.extracted_camera_entity)
        else {
            continue;
        };

        let Ok(view) = camera_views.get(default_camera_view.0) else {
            continue;
        };

        let Some(transparent_phase) = transparent_render_phases.get_mut(&view.retained_view_entity)
        else {
            continue;
        };

        let pipeline = pipelines.specialize(
            &pipeline_cache,
            &ui_material_pipeline,
            UiMaterialKey {
                hdr: view.hdr,
                bind_group_data: material.key.clone(),
            },
        );
        if transparent_phase.items.capacity() < extracted_spans.len() {
            transparent_phase
                .items
                .reserve_exact(extracted_spans.len() - transparent_phase.items.capacity());
        }
        transparent_phase.add(TransparentUi {
            draw_function,
            pipeline,
            entity: (extracted_span.render_entity, extracted_span.main_entity),
            sort_key: FloatOrd(
                extracted_span.stack_index as f32 + bevy::ui::stack_z_offsets::MATERIAL,
            ),
            batch_range: 0..0,
            extra_index: PhaseItemExtraIndex::None,
            index,
            indexed: false,
        });
    }
}
