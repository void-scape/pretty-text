use bevy::{
    asset::{RenderAssetUsages, weak_handle},
    core_pipeline::core_2d::{CORE_2D_DEPTH_FORMAT, Transparent2d},
    ecs::system::{SystemParamItem, SystemState, lifetimeless::SRes},
    math::FloatOrd,
    prelude::*,
    render::{
        Extract, Render, RenderApp, RenderSet,
        mesh::{Indices, MeshVertexAttribute, PrimitiveTopology, RenderMesh},
        render_asset::{PrepareAssetError, RenderAsset, RenderAssetPlugin, RenderAssets},
        render_phase::{
            AddRenderCommand, DrawFunctions, PhaseItem, PhaseItemExtraIndex, RenderCommand,
            RenderCommandResult, SetItemPipeline, TrackedRenderPass, ViewSortedRenderPhases,
        },
        render_resource::{
            AsBindGroup, AsBindGroupError, BindGroup, BindGroupLayout, BlendState,
            ColorTargetState, ColorWrites, CompareFunction, DepthBiasState, DepthStencilState,
            Face, FragmentState, FrontFace, MultisampleState, PipelineCache, PolygonMode,
            PrimitiveState, RenderPipelineDescriptor, SpecializedRenderPipeline,
            SpecializedRenderPipelines, StencilFaceState, StencilState, TextureFormat,
            VertexBufferLayout, VertexFormat, VertexState, VertexStepMode,
        },
        renderer::RenderDevice,
        sync_component::SyncComponentPlugin,
        sync_world::{MainEntityHashMap, RenderEntity},
        view::{ExtractedView, RenderLayers, RenderVisibleEntities, ViewTarget, VisibilitySystems},
    },
    sprite::{
        Anchor, DrawMesh2d, Mesh2dPipeline, Mesh2dPipelineKey, Mesh2dTransforms, MeshFlags,
        SetMesh2dBindGroup, SetMesh2dViewBindGroup, alpha_mode_pipeline_key, extract_mesh2d,
    },
    text::{GlyphAtlasLocation, TextBounds, TextLayoutInfo, Update2dText},
    window::PrimaryWindow,
};

use crate::PrettyText;

pub const GLYPH_SHADER_HANDLE: Handle<Shader> =
    weak_handle!("35d4f25c-eb2b-4f26-872f-ef666a76554e");

const GLYPH_SHADER: &str = r"
#import bevy_sprite::mesh2d_functions

struct Vertex {
    @builtin(instance_index) instance_index: u32,
    @location(0) position: vec3<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) atlas_uv: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@group(2) @binding(0) var texture: texture_2d<f32>;
@group(2) @binding(1) var texture_sampler: sampler;

@vertex
fn vertex(vertex: Vertex) -> VertexOutput {
    var out: VertexOutput;
    let model = mesh2d_functions::get_world_from_local(vertex.instance_index);
    out.clip_position = mesh2d_functions::mesh2d_position_local_to_clip(model, vec4<f32>(vertex.position, 1.0));

    out.uv = vertex.atlas_uv;

    return out;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(texture, texture_sampler, in.uv);
}
";

pub struct GlyphMeshPlugin;

impl Plugin for GlyphMeshPlugin {
    fn build(&self, app: &mut App) {
        let mut shaders = app.world_mut().resource_mut::<Assets<Shader>>();
        shaders.insert(
            &GLYPH_SHADER_HANDLE,
            Shader::from_wgsl(GLYPH_SHADER, file!()),
        );
        app.add_plugins((
            RenderAssetPlugin::<PreparedGlyphAtlas>::default(),
            SyncComponentPlugin::<GlyphMesh2d>::default(),
        ))
        .init_asset::<AtlasUniform>()
        .add_systems(
            PostUpdate,
            (
                force_hidden
                    .before(VisibilitySystems::VisibilityPropagate)
                    .before(Update2dText),
                (
                    gliphify_text2d.after(Update2dText),
                    position_glyphs.after(TransformSystem::TransformPropagate),
                )
                    .chain(),
            ),
        );

        app.get_sub_app_mut(RenderApp)
            .unwrap()
            .add_render_command::<Transparent2d, DrawGlyphMesh2d>()
            .init_resource::<SpecializedRenderPipelines<GlyphMesh2dPipeline>>()
            .init_resource::<RenderGlyphMesh2dInstances>()
            .add_systems(ExtractSchedule, extract_glyph_mesh2d.after(extract_mesh2d))
            .add_systems(Render, queue_glyph_mesh2d.in_set(RenderSet::QueueMeshes));
    }

    fn finish(&self, app: &mut App) {
        app.get_sub_app_mut(RenderApp)
            .unwrap()
            .init_resource::<GlyphMesh2dPipeline>();
    }
}

#[derive(Default, Component)]
pub struct GlyphCount(pub usize);

#[derive(Default, Component)]
pub(crate) struct OrderedGlyphs(pub Vec<Entity>);

#[derive(Clone, Component)]
struct GlyphMesh2d {
    atlas: Handle<AtlasUniform>,
}

#[derive(Clone, Asset, TypePath, AsBindGroup)]
struct AtlasUniform {
    #[texture(0)]
    #[sampler(1)]
    atlas: Handle<Image>,
}

#[derive(Component)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub(crate) struct Glyphs(Vec<Entity>);

#[derive(Component)]
#[relationship(relationship_target = Glyphs)]
pub(crate) struct GlyphOf(Entity);

pub(crate) fn gliphify_text2d(
    mut commands: Commands,
    server: Res<AssetServer>,
    mut text2d: Query<
        (
            Entity,
            &mut OrderedGlyphs,
            &GlobalTransform,
            &TextLayoutInfo,
            &TextBounds,
            &Anchor,
            Option<&RenderLayers>,
        ),
        (Changed<TextLayoutInfo>, With<PrettyText>),
    >,
    mut meshes: ResMut<Assets<Mesh>>,
    windows: Query<&Window, With<PrimaryWindow>>,
    atlases: Res<Assets<TextureAtlasLayout>>,
) -> Result {
    let scale_factor = windows
        .single()
        .map(|window| window.resolution.scale_factor())
        .unwrap_or(1.0);
    let scaling = GlobalTransform::from_scale(Vec2::splat(scale_factor.recip()).extend(1.));

    for (entity, mut ordered, gt, layout, text_bounds, anchor, layers) in text2d.iter_mut() {
        commands
            .entity(entity)
            .despawn_related::<Glyphs>()
            .insert(GlyphCount(layout.glyphs.len()));
        ordered.0.clear();

        let layers = layers.cloned().unwrap_or_default();
        for (i, glyph) in layout.glyphs.iter().enumerate() {
            // TODO: handle this!
            let atlas = atlases
                .get(&glyph.atlas_info.texture_atlas)
                .ok_or("failed to turn `Text2d` into glyphs: font atlas has not loaded yet")?;

            let size = Vec2::new(
                text_bounds.width.unwrap_or(layout.size.x),
                text_bounds.height.unwrap_or(layout.size.y),
            );
            let bottom_left = -(anchor.as_vec() + 0.5) * size + (size.y - layout.size.y) * Vec2::Y;
            // TODO: z ordering?
            let transform = *gt
                * GlobalTransform::from_translation(bottom_left.extend(0.))
                * scaling
                * GlobalTransform::from_translation(glyph.position.extend(i as f32 * 0.001));

            let id = commands
                .spawn((
                    GlyphOf(entity),
                    GlyphMesh2d {
                        atlas: server.add(AtlasUniform {
                            atlas: glyph.atlas_info.texture.clone(),
                        }),
                    },
                    Mesh2d(meshes.add(glyph_mesh(
                        glyph.size.x,
                        glyph.size.y,
                        atlas,
                        &glyph.atlas_info.location,
                    ))),
                    GlyphPosition(glyph.position),
                    transform.compute_transform(),
                    transform,
                    layers.clone(),
                ))
                .id();
            ordered.0.push(id);
        }
    }

    Ok(())
}

fn glyph_mesh(
    width: f32,
    height: f32,
    atlas: &TextureAtlasLayout,
    location: &GlyphAtlasLocation,
) -> Mesh {
    let [hw, hh] = [width / 2., height / 2.];
    let positions = vec![
        [hw, hh, 0.0],
        [-hw, hh, 0.0],
        [-hw, -hh, 0.0],
        [hw, -hh, 0.0],
    ];
    let uvs = vec![[1.0, 0.0], [0.0, 0.0], [0.0, 1.0], [1.0, 1.0]];
    let indices = Indices::U32(vec![0, 1, 2, 0, 2, 3]);

    let rect = atlas.textures[location.glyph_index];
    let min = rect.min.as_vec2() / atlas.size.as_vec2();
    let max = rect.max.as_vec2() / atlas.size.as_vec2();

    Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    )
    .with_inserted_indices(indices)
    .with_inserted_attribute(
        MeshVertexAttribute::new("Vertex_Position", 0, VertexFormat::Float32x3),
        positions,
    )
    .with_inserted_attribute(
        MeshVertexAttribute::new("Vertex_Uvs", 1, VertexFormat::Float32x2),
        uvs,
    )
    .with_inserted_attribute(
        MeshVertexAttribute::new("Vertex_Glyph_Atlas_Uvs", 2, VertexFormat::Float32x2),
        vec![
            vec2(max.x, min.y), // tr
            vec2(min.x, min.y), // tl
            vec2(min.x, max.y), // bl
            vec2(max.x, max.y), // br
        ],
    )
}

#[derive(Component)]
struct GlyphPosition(Vec2);

// Spawning glyphs as children of Text2d will cause the layout to recompute ... looping
// infinitely ...
fn position_glyphs(
    mut transforms: Query<(&mut Transform, &GlyphPosition), With<GlyphOf>>,
    roots: Query<
        (
            &GlobalTransform,
            &Glyphs,
            &TextLayoutInfo,
            &TextBounds,
            &Anchor,
        ),
        (Without<GlyphOf>, Changed<GlobalTransform>),
    >,
    windows: Query<&Window, With<PrimaryWindow>>,
) {
    let scale_factor = windows
        .single()
        .map(|window| window.resolution.scale_factor())
        .unwrap_or(1.0);
    let scaling = GlobalTransform::from_scale(Vec2::splat(scale_factor.recip()).extend(1.));

    for (gt, glyphs, layout, text_bounds, anchor) in roots.iter() {
        let size = Vec2::new(
            text_bounds.width.unwrap_or(layout.size.x),
            text_bounds.height.unwrap_or(layout.size.y),
        );
        let bottom_left = -(anchor.as_vec() + 0.5) * size + (size.y - layout.size.y) * Vec2::Y;

        let mut iter = transforms.iter_many_mut(glyphs.iter());
        let mut i = 0;
        while let Some((mut transform, position)) = iter.fetch_next() {
            // TODO: z ordering?
            *transform = (*gt
                * GlobalTransform::from_translation(bottom_left.extend(0.))
                * scaling
                * GlobalTransform::from_translation(position.0.extend(i as f32 * 0.001)))
            .compute_transform();
            i += 1;
        }
    }
}

// PrettyText entities *must* be hidden otherwise text will be rendered here and in the default Text2d
// pipeline ...
fn force_hidden(mut vis: Query<&mut Visibility, With<PrettyText>>) {
    for mut vis in vis
        .iter_mut()
        .filter(|vis| !matches!(vis.as_ref(), Visibility::Hidden))
    {
        *vis = Visibility::Hidden;
    }
}

#[derive(Resource)]
pub struct GlyphMesh2dPipeline {
    mesh2d_pipeline: Mesh2dPipeline,
    atlas_uniform_layout: BindGroupLayout,
}

impl FromWorld for GlyphMesh2dPipeline {
    fn from_world(world: &mut World) -> Self {
        let mut system_state: SystemState<Res<RenderDevice>> = SystemState::new(world);
        let render_device = system_state.get_mut(world);
        let render_device = render_device.into_inner();

        Self {
            atlas_uniform_layout: AtlasUniform::bind_group_layout(render_device),
            mesh2d_pipeline: Mesh2dPipeline::from_world(world),
        }
    }
}

impl SpecializedRenderPipeline for GlyphMesh2dPipeline {
    type Key = Mesh2dPipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let formats = vec![
            // Position
            VertexFormat::Float32x3,
            // Uv
            VertexFormat::Float32x2,
            // Glyph Atlas Uv
            VertexFormat::Float32x2,
        ];

        let vertex_layout =
            VertexBufferLayout::from_vertex_formats(VertexStepMode::Vertex, formats);

        let format = match key.contains(Mesh2dPipelineKey::HDR) {
            true => ViewTarget::TEXTURE_FORMAT_HDR,
            false => TextureFormat::bevy_default(),
        };

        RenderPipelineDescriptor {
            vertex: VertexState {
                shader: GLYPH_SHADER_HANDLE,
                entry_point: "vertex".into(),
                shader_defs: vec![],
                buffers: vec![vertex_layout],
            },
            fragment: Some(FragmentState {
                shader: GLYPH_SHADER_HANDLE,
                shader_defs: vec![],
                entry_point: "fragment".into(),
                targets: vec![Some(ColorTargetState {
                    format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            layout: vec![
                self.mesh2d_pipeline.view_layout.clone(),
                self.mesh2d_pipeline.mesh_layout.clone(),
                self.atlas_uniform_layout.clone(),
            ],
            push_constant_ranges: vec![],
            primitive: PrimitiveState {
                front_face: FrontFace::Ccw,
                cull_mode: Some(Face::Back),
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
                topology: key.primitive_topology(),
                strip_index_format: None,
            },
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
            label: Some("glyph_mesh2d_pipeline".into()),
            zero_initialize_workgroup_memory: false,
        }
    }
}

type DrawGlyphMesh2d = (
    SetItemPipeline,
    SetMesh2dViewBindGroup<0>,
    SetMesh2dBindGroup<1>,
    SetGlyphAtlasBindGroup<2>,
    DrawMesh2d,
);

struct SetGlyphAtlasBindGroup<const I: usize>;
impl<P: PhaseItem, const I: usize> RenderCommand<P> for SetGlyphAtlasBindGroup<I> {
    type Param = (
        SRes<RenderAssets<PreparedGlyphAtlas>>,
        SRes<RenderGlyphMesh2dInstances>,
    );
    type ViewQuery = ();
    type ItemQuery = ();

    #[inline]
    fn render<'w>(
        item: &P,
        _view: (),
        _item_query: Option<()>,
        (glyphs, glyph_instances): SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let glyphs = glyphs.into_inner();
        let glyph_instances = glyph_instances.into_inner();
        let Some(glyph_instance) = glyph_instances.get(&item.main_entity()) else {
            return RenderCommandResult::Skip;
        };
        let Some(glyph_atlas) = glyphs.get(glyph_instance.glyph_atlas) else {
            return RenderCommandResult::Skip;
        };
        pass.set_bind_group(I, &glyph_atlas.bind_group, &[]);
        RenderCommandResult::Success
    }
}

struct PreparedGlyphAtlas {
    bind_group: BindGroup,
}

impl RenderAsset for PreparedGlyphAtlas {
    type SourceAsset = AtlasUniform;

    type Param = (
        SRes<RenderDevice>,
        SRes<GlyphMesh2dPipeline>,
        <AtlasUniform as AsBindGroup>::Param,
    );

    fn prepare_asset(
        material: Self::SourceAsset,
        _: AssetId<Self::SourceAsset>,
        (render_device, pipeline, material_param): &mut SystemParamItem<Self::Param>,
    ) -> Result<Self, PrepareAssetError<Self::SourceAsset>> {
        match material.as_bind_group(
            &pipeline.atlas_uniform_layout,
            render_device,
            material_param,
        ) {
            Ok(prepared) => {
                let mut mesh_pipeline_key_bits = Mesh2dPipelineKey::empty();
                mesh_pipeline_key_bits
                    .insert(alpha_mode_pipeline_key(bevy::sprite::AlphaMode2d::Blend));

                Ok(PreparedGlyphAtlas {
                    bind_group: prepared.bind_group,
                })
            }
            Err(AsBindGroupError::RetryNextUpdate) => {
                Err(PrepareAssetError::RetryNextUpdate(material))
            }
            Err(other) => Err(PrepareAssetError::AsBindGroupError(other)),
        }
    }
}

#[derive(Resource, Deref, DerefMut, Default)]
struct RenderGlyphMesh2dInstances(MainEntityHashMap<RenderGlyphInstance>);

struct RenderGlyphInstance {
    transforms: Mesh2dTransforms,
    mesh_asset_id: AssetId<Mesh>,
    glyph_atlas: AssetId<AtlasUniform>,
}

fn extract_glyph_mesh2d(
    mut commands: Commands,
    mut previous_len: Local<usize>,
    query: Extract<
        Query<(
            Entity,
            RenderEntity,
            &ViewVisibility,
            &GlobalTransform,
            &Mesh2d,
            &GlyphMesh2d,
        )>,
    >,
    mut render_mesh_instances: ResMut<RenderGlyphMesh2dInstances>,
) {
    let mut values = Vec::with_capacity(*previous_len);
    for (entity, render_entity, view_visibility, transform, handle, glyph) in &query {
        if !view_visibility.get() {
            continue;
        }

        let transforms = Mesh2dTransforms {
            world_from_local: (&transform.affine()).into(),
            flags: MeshFlags::empty().bits(),
        };

        // TODO: cloning the handle every frame?
        values.push((render_entity, glyph.clone()));
        render_mesh_instances.insert(
            entity.into(),
            RenderGlyphInstance {
                mesh_asset_id: handle.0.id(),
                glyph_atlas: glyph.atlas.id(),
                transforms,
            },
        );
    }
    *previous_len = values.len();
    commands.try_insert_batch(values);
}

fn queue_glyph_mesh2d(
    transparent_draw_functions: Res<DrawFunctions<Transparent2d>>,
    glyph_mesh2d_pipeline: Res<GlyphMesh2dPipeline>,
    mut pipelines: ResMut<SpecializedRenderPipelines<GlyphMesh2dPipeline>>,
    pipeline_cache: Res<PipelineCache>,
    render_meshes: Res<RenderAssets<RenderMesh>>,
    render_mesh_instances: Res<RenderGlyphMesh2dInstances>,
    mut transparent_render_phases: ResMut<ViewSortedRenderPhases<Transparent2d>>,
    views: Query<(&RenderVisibleEntities, &ExtractedView, &Msaa)>,
) {
    if render_mesh_instances.is_empty() {
        return;
    }

    for (visible_entities, view, msaa) in &views {
        let Some(transparent_phase) = transparent_render_phases.get_mut(&view.retained_view_entity)
        else {
            continue;
        };

        let draw_glyph_mesh2d = transparent_draw_functions.read().id::<DrawGlyphMesh2d>();

        let mesh_key = Mesh2dPipelineKey::from_msaa_samples(msaa.samples())
            | Mesh2dPipelineKey::from_hdr(view.hdr);

        for (render_entity, visible_entity) in visible_entities.iter::<Mesh2d>() {
            if let Some(mesh_instance) = render_mesh_instances.get(visible_entity) {
                let mesh2d_handle = mesh_instance.mesh_asset_id;
                let mesh2d_transforms = &mesh_instance.transforms;

                let mut mesh2d_key = mesh_key;
                let Some(mesh) = render_meshes.get(mesh2d_handle) else {
                    continue;
                };
                mesh2d_key |= Mesh2dPipelineKey::from_primitive_topology(mesh.primitive_topology());

                let pipeline_id =
                    pipelines.specialize(&pipeline_cache, &glyph_mesh2d_pipeline, mesh2d_key);

                let mesh_z = mesh2d_transforms.world_from_local.translation.z;
                transparent_phase.add(Transparent2d {
                    entity: (*render_entity, *visible_entity),
                    draw_function: draw_glyph_mesh2d,
                    pipeline: pipeline_id,
                    sort_key: FloatOrd(mesh_z),
                    batch_range: 0..1,
                    extra_index: PhaseItemExtraIndex::None,
                    extracted_index: usize::MAX,
                    indexed: mesh.indexed(),
                });
            }
        }
    }
}
