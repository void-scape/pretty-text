use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Range;

use bevy::ecs::system::SystemParamItem;
use bevy::math::{Mat4, Rect};
use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use bevy::render::RenderApp;
use bevy::render::mesh::{VertexBufferLayout, VertexFormat};
use bevy::render::render_phase::{RenderCommandResult, TrackedRenderPass};
use bevy::render::render_resource::{BindGroup, BufferUsages, RawBufferVec, VertexStepMode};
use bevy::render::sync_world::MainEntity;
use bevy::render::{Extract, Render, RenderSet, extract_component::ExtractComponentPlugin};
use bevy::{
    ecs::{
        prelude::Component,
        query::ROQueryItem,
        system::lifetimeless::{Read, SRes},
    },
    render::render_phase::{PhaseItem, RenderCommand},
};

use crate::effects::material::PrettyTextMaterial;
use crate::prelude::GlyphMaterial;

mod r2d;
mod ui;

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
        ));

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .init_resource::<ExtractedGlyphSpans<T>>()
                .init_resource::<ExtractedGlyphs>()
                .init_resource::<GlyphMaterialMeta<T>>()
                .add_systems(
                    Render,
                    (
                        prepare_glyph_meta::<T>.in_set(RenderSet::PrepareBindGroups),
                        clear_spans::<T>.in_set(RenderSet::Cleanup),
                    ),
                );
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
        meta: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(batch) = batch else {
            return RenderCommandResult::Skip;
        };

        let mut vertices = batch.range.clone();
        vertices.start *= 6;
        vertices.end *= 6;

        let meta = meta.into_inner();
        pass.set_vertex_buffer(0, meta.vertices.buffer().unwrap().slice(..));
        pass.set_vertex_buffer(1, meta.instances.buffer().unwrap().slice(..));
        pass.draw(vertices, batch.range.clone());
        RenderCommandResult::Success
    }
}

#[derive(Resource)]
struct GlyphMaterialMeta<M: GlyphMaterial> {
    vertices: RawBufferVec<GlyphVertex>,
    instances: RawBufferVec<GlyphInstance>,
    marker: PhantomData<M>,
}

impl<M: GlyphMaterial> Default for GlyphMaterialMeta<M> {
    fn default() -> Self {
        Self {
            vertices: RawBufferVec::new(BufferUsages::VERTEX),
            instances: RawBufferVec::new(BufferUsages::VERTEX),
            marker: PhantomData,
        }
    }
}

fn prepare_glyph_meta<T: GlyphMaterial>(mut glyph_meta: ResMut<GlyphMaterialMeta<T>>) {
    glyph_meta.vertices.clear();
    glyph_meta.instances.clear();
}

#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct GlyphVertex {
    position: [f32; 3],
    uv: [f32; 2],
    color: [f32; 4],
}

#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct GlyphInstance {
    span_color: [f32; 4],
    scale: [f32; 2],
    index: u32,
}

fn vertex_buffer_layouts() -> [VertexBufferLayout; 2] {
    let vertex_layout = VertexBufferLayout::from_vertex_formats(
        VertexStepMode::Vertex,
        [
            VertexFormat::Float32x3,
            VertexFormat::Float32x2,
            VertexFormat::Float32x4,
        ],
    );
    let instance_layout = VertexBufferLayout::from_vertex_formats(
        VertexStepMode::Instance,
        [
            VertexFormat::Float32x4,
            VertexFormat::Float32x2,
            VertexFormat::Uint32,
        ],
    )
    .offset_locations_by(3);

    [vertex_layout, instance_layout]
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
    vertices: [Mat4; 4],
    colors: [[f32; 4]; 4],
    rect: Rect,
    glyph_scale: Vec2,
    index: u32,
}

fn clear_glyphs(mut extracted_glyphs: ResMut<ExtractedGlyphs>) {
    extracted_glyphs.clear();
}

fn clear_spans<T: GlyphMaterial>(mut extracted_spans: ResMut<ExtractedGlyphSpans<T>>) {
    extracted_spans.0.clear();
}
