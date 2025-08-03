use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Range;

use bevy::ecs::system::SystemParamItem;
use bevy::math::{FloatOrd, Mat4, Rect};
use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::render::RenderApp;
use bevy::render::globals::GlobalsBuffer;
use bevy::render::render_phase::{
    AddRenderCommand, DrawFunctions, PhaseItemExtraIndex, RenderCommandResult, TrackedRenderPass,
    ViewSortedRenderPhases,
};
use bevy::render::sync_world::MainEntity;
use bevy::render::{
    Extract, Render, RenderSet,
    extract_component::ExtractComponentPlugin,
    globals::GlobalsUniform,
    render_asset::{PrepareAssetError, RenderAsset, RenderAssetPlugin, RenderAssets},
    render_resource::{binding_types::uniform_buffer, *},
    renderer::RenderDevice,
    view::*,
};
use bevy::ui::{TransparentUi, UiCameraView};
use bevy::{
    ecs::{
        prelude::Component,
        query::ROQueryItem,
        system::lifetimeless::{Read, SRes},
    },
    render::render_phase::{PhaseItem, RenderCommand, SetItemPipeline},
};

use crate::effects::material::{DEFAULT_GLYPH_SHADER_HANDLE, PrettyTextMaterial};
use crate::prelude::GlyphMaterial;

mod r2d;
mod ui;

/// Inline shader as an `embedded_asset` and load it permanently.
///
/// This works around a limitation of the shader loader not properly loading
/// dependencies of shaders.
#[macro_export]
macro_rules! load_shader_library {
    ($asset_server_provider: expr, $path: literal $(, $settings: expr)?) => {
        $crate::_macro::bevy_asset::embedded_asset!($asset_server_provider, $path);
        let handle: $crate::_macro::bevy_asset::prelude::Handle<$crate::prelude::Shader> =
            $crate::_macro::bevy_asset::load_embedded_asset!(
                $asset_server_provider,
                $path
                $(,$settings)?
            );
        core::mem::forget(handle);
    }
}

pub(super) fn plugin(app: &mut App) {
    if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
        render_app
            .init_resource::<ImageBindGroups>()
            .init_resource::<ImageAssetEvents>()
            .add_systems(Render, clear_glyphs.in_set(RenderSet::Cleanup));

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_systems(ExtractSchedule, extract_image_events)
                .add_systems(
                    Render,
                    handle_image_events.before(RenderSet::PrepareBindGroups),
                );
        }
    }
}

/// Adds the necessary ECS resources and render logic to enable rendering entities
/// using the given [`GlyphMaterial`] asset type.
#[derive(Debug)]
pub struct GlyphMaterialPlugin<T: GlyphMaterial>(PhantomData<T>);

impl<T: GlyphMaterial> Default for GlyphMaterialPlugin<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: GlyphMaterial> Plugin for GlyphMaterialPlugin<T>
where
    T::Data: PartialEq + Eq + Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.init_asset::<T>().add_plugins((
            ui::GlyphMaterialUiPlugin::<T>::default(),
            r2d::GlyphMaterial2dPlugin::<T>::default(),
            ExtractComponentPlugin::<PrettyTextMaterial<T>>::extract_visible(),
            RenderAssetPlugin::<PreparedGlyphMaterial<T>>::default(),
        ));

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_render_command::<TransparentUi, DrawGlyphMaterial<T>>()
                .init_resource::<GlyphMaterialMeta<T>>()
                .init_resource::<ExtractedGlyphSpans<T>>()
                .init_resource::<ExtractedGlyphs>()
                .init_resource::<SpecializedRenderPipelines<GlyphMaterialPipeline<T>>>()
                .add_systems(
                    Render,
                    (
                        prepare_glyph_meta::<T>.in_set(RenderSet::PrepareBindGroups),
                        queue_glyphs::<T>.in_set(RenderSet::Queue),
                    ),
                );
        }
    }

    fn finish(&self, app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app.init_resource::<GlyphMaterialPipeline<T>>();
        }
    }
}

/// Render pipeline data for a given [`GlyphMaterial`].
#[derive(Resource)]
struct GlyphMaterialPipeline<M: GlyphMaterial> {
    view_layout: BindGroupLayout,
    texture_layout: BindGroupLayout,
    material_layout: BindGroupLayout,
    vertex_shader: Option<Handle<Shader>>,
    fragment_shader: Option<Handle<Shader>>,
    marker: PhantomData<M>,
}

impl<M: GlyphMaterial> SpecializedRenderPipeline for GlyphMaterialPipeline<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    type Key = MaterialKey<M>;

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
        let shader_defs = Vec::new();

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

        descriptor.layout = vec![
            self.view_layout.clone(),
            self.texture_layout.clone(),
            self.material_layout.clone(),
        ];

        descriptor
    }
}

impl<M: GlyphMaterial> FromWorld for GlyphMaterialPipeline<M> {
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

        GlyphMaterialPipeline {
            view_layout,
            texture_layout,
            material_layout,
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

struct MaterialKey<T: GlyphMaterial> {
    hdr: bool,
    bind_group_data: T::Data,
}

impl<T: GlyphMaterial> Eq for MaterialKey<T> where T::Data: PartialEq {}

impl<T: GlyphMaterial> PartialEq for MaterialKey<T>
where
    T::Data: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.hdr == other.hdr && self.bind_group_data == other.bind_group_data
    }
}

impl<T: GlyphMaterial> Clone for MaterialKey<T>
where
    T::Data: Clone,
{
    fn clone(&self) -> Self {
        Self {
            hdr: self.hdr,
            bind_group_data: self.bind_group_data.clone(),
        }
    }
}

impl<T: GlyphMaterial> Hash for MaterialKey<T>
where
    T::Data: Hash,
{
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.hdr.hash(state);
        self.bind_group_data.hash(state);
    }
}

type DrawGlyphMaterial<M> = (
    SetItemPipeline,
    SetViewBindGroup<M, 0>,
    SetTextureBindGroup<M, 1>,
    SetMaterialBindGroup<M, 2>,
    DrawGlyph<M>,
);

struct SetViewBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P> for SetViewBindGroup<M, I> {
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

struct SetTextureBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P>
    for SetTextureBindGroup<M, I>
{
    type Param = SRes<ImageBindGroups>;
    type ViewQuery = ();
    type ItemQuery = Read<GlyphBatch<M>>;

    fn render<'w>(
        _item: &P,
        _view: (),
        batch: Option<ROQueryItem<'w, Self::ItemQuery>>,
        bind_groups: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(batch) = batch else {
            return RenderCommandResult::Skip;
        };

        pass.set_bind_group(
            I,
            bind_groups.into_inner().0.get(&batch.image).unwrap(),
            &[],
        );
        RenderCommandResult::Success
    }
}

struct SetMaterialBindGroup<M: GlyphMaterial, const I: usize>(PhantomData<M>);
impl<P: PhaseItem, M: GlyphMaterial, const I: usize> RenderCommand<P>
    for SetMaterialBindGroup<M, I>
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

fn prepare_glyph_meta<T: GlyphMaterial>(
    render_device: Res<RenderDevice>,
    view_uniforms: Res<ViewUniforms>,
    globals_buffer: Res<GlobalsBuffer>,
    material_pipeline: Res<GlyphMaterialPipeline<T>>,
    mut glyph_meta: ResMut<GlyphMaterialMeta<T>>,
) {
    if let (Some(view_binding), Some(globals_binding)) = (
        view_uniforms.uniforms.binding(),
        globals_buffer.buffer.binding(),
    ) {
        glyph_meta.view_bind_group = Some(render_device.create_bind_group(
            "pretty_text_glyph_view_bind_group",
            &material_pipeline.view_layout,
            &BindGroupEntries::sequential((view_binding, globals_binding)),
        ));
        glyph_meta.vertices.clear();
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
    image: AssetId<Image>,
}

#[derive(Default, Resource)]
struct ImageBindGroups(HashMap<AssetId<Image>, BindGroup>);

#[derive(Default, Resource)]
struct ImageAssetEvents(Vec<AssetEvent<Image>>);

fn extract_image_events(
    mut events: ResMut<ImageAssetEvents>,
    mut image_events: Extract<EventReader<AssetEvent<Image>>>,
) {
    let ImageAssetEvents(ref mut images) = *events;
    images.clear();

    for event in image_events.read() {
        images.push(*event);
    }
}

fn handle_image_events(
    mut image_bind_groups: ResMut<ImageBindGroups>,
    events: Res<ImageAssetEvents>,
) {
    for event in &events.0 {
        match event {
            AssetEvent::Added { .. } |
            // Images don't have dependencies
            AssetEvent::LoadedWithDependencies { .. } => {}
            AssetEvent::Unused { id } | AssetEvent::Modified { id } | AssetEvent::Removed { id } => {
                image_bind_groups.0.remove(id);
            }
        };
    }
}

/// Spans of `ExtractedGlyph` instances.
#[derive(Deref, DerefMut, Resource)]
struct ExtractedGlyphSpans<M: GlyphMaterial>(Vec<ExtractedGlyphSpan<M>>);

impl<M: GlyphMaterial> Default for ExtractedGlyphSpans<M> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

/// A span of `ExtractedGlyph` instances.
///
/// This represents a `GlyphSpan`.
struct ExtractedGlyphSpan<M: GlyphMaterial> {
    kind: ExtractedGlyphSpanKind,
    sork_key: f32,
    main_entity: MainEntity,
    render_entity: Entity,
    color: [f32; 4],
    image: AssetId<Image>,
    material: AssetId<M>,
    extracted: Vec<usize>,
}

enum ExtractedGlyphSpanKind {
    Sprite,
    Ui {
        clip: Option<Rect>,
        // Camera to render this UI node to. By the time it is extracted,
        // it is defaulted to a single camera if only one exists.
        // Nodes with ambiguous camera will be ignored.
        extracted_camera_entity: Entity,
    },
}

/// Collection of `Glyph`s.
#[derive(Default, Deref, DerefMut, Resource)]
struct ExtractedGlyphs(Vec<ExtractedGlyph>);

/// Represents a `Glyph`.
#[derive(Clone, Copy)]
struct ExtractedGlyph {
    transform: Mat4,
    rect: Rect,
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
        match material.as_bind_group(&pipeline.material_layout, render_device, material_param) {
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
    material_pipeline: Res<GlyphMaterialPipeline<M>>,
    mut pipelines: ResMut<SpecializedRenderPipelines<GlyphMaterialPipeline<M>>>,
    pipeline_cache: Res<PipelineCache>,
    render_materials: Res<RenderAssets<PreparedGlyphMaterial<M>>>,
    mut transparent_render_phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut render_views: Query<&UiCameraView, With<ExtractedView>>,
    camera_views: Query<&ExtractedView>,
    views: Query<&ExtractedView>,
) where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    let draw_function = draw_functions.read().id::<DrawGlyphMaterial<M>>();

    for (index, extracted_span) in extracted_spans.iter().enumerate() {
        let Some(material) = render_materials.get(extracted_span.material) else {
            continue;
        };

        match extracted_span.kind {
            ExtractedGlyphSpanKind::Sprite => {
                for view in views.iter() {
                    let Some(transparent_phase) =
                        transparent_render_phases.get_mut(&view.retained_view_entity)
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

                    // TODO: clip if not visible
                    //
                    // https://github.com/bevyengine/bevy/blob/main/crates/bevy_sprite/src/render/mod.rs#L575
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
                    transparent_render_phases.get_mut(&view.retained_view_entity)
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

fn clear_glyphs(mut extracted_glyphs: ResMut<ExtractedGlyphs>) {
    extracted_glyphs.clear();
}
