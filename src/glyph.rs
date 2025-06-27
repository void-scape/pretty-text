use bevy::{
    asset::{RenderAssetUsages, load_internal_asset, weak_handle},
    prelude::*,
    render::{
        mesh::{Indices, PrimitiveTopology},
        render_resource::{AsBindGroup, ShaderRef},
        view::VisibilitySystems,
    },
    sprite::{AlphaMode2d, Anchor, Material2d, Material2dPlugin},
    text::{GlyphAtlasLocation, TextBounds, TextLayoutInfo, Update2dText},
    window::PrimaryWindow,
};

use crate::PrettyText;

const GLYPH_SHADER_HANDLE: Handle<Shader> = weak_handle!("35d4f25c-eb2b-4f26-872f-ef666a76554e");

#[derive(Debug, SystemSet, PartialEq, Eq, Hash, Clone)]
pub enum GlyphSystems {
    Construct,
    Position,
}

pub struct GlyphMeshPlugin;

impl Plugin for GlyphMeshPlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            GLYPH_SHADER_HANDLE,
            "shaders/default_glyph_material.wgsl",
            Shader::from_wgsl
        );

        app.add_plugins(Material2dPlugin::<DefaultGlyphMaterial>::default())
            .add_systems(
                PostUpdate,
                (
                    gliphify_text2d
                        .after(Update2dText)
                        .in_set(GlyphSystems::Construct),
                    glyph_transform_propogate
                        .before(TransformSystem::TransformPropagate)
                        .in_set(GlyphSystems::Position),
                    hide_builtin_text
                        .in_set(VisibilitySystems::CheckVisibility)
                        .after(bevy::render::view::check_visibility),
                ),
            );
    }
}

#[derive(Clone, Asset, TypePath, AsBindGroup)]
pub struct DefaultGlyphMaterial {
    #[texture(0)]
    #[sampler(1)]
    pub atlas: Handle<Image>,
}

impl Material2d for DefaultGlyphMaterial {
    fn vertex_shader() -> ShaderRef {
        ShaderRef::Handle(GLYPH_SHADER_HANDLE)
    }

    fn fragment_shader() -> ShaderRef {
        ShaderRef::Handle(GLYPH_SHADER_HANDLE)
    }

    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Blend
    }
}

#[derive(Default, Component)]
#[component(immutable)]
pub struct GlyphCount(pub usize);

#[derive(Default, Component)]
pub struct OrderedGlyphs(Vec<Entity>);

impl OrderedGlyphs {
    pub fn entities(&self) -> &[Entity] {
        &self.0
    }
}

#[derive(Component)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub struct Glyphs(Vec<Entity>);

#[derive(Component)]
#[relationship(relationship_target = Glyphs)]
pub struct GlyphOf(pub Entity);

fn gliphify_text2d(
    mut commands: Commands,
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
    mut materials: ResMut<Assets<DefaultGlyphMaterial>>,
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
                    MeshMaterial2d(materials.add(DefaultGlyphMaterial {
                        atlas: glyph.atlas_info.texture.clone(),
                    })),
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

#[derive(Component)]
struct GlyphPosition(Vec2);

// Spawning glyphs as children of `Text2d` will cause the layout to recompute ... looping
// infinitely!
fn glyph_transform_propogate(
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

// `PrettyText` entites *must* be hidden otherwise text will be rendered here and in the default Text2d
// pipeline.
fn hide_builtin_text(mut vis: Query<&mut ViewVisibility, With<PrettyText>>) {
    for mut vis in vis.iter_mut() {
        *vis = ViewVisibility::HIDDEN;
    }
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
    .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
    .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
    .with_inserted_attribute(
        Mesh::ATTRIBUTE_COLOR,
        vec![
            vec4(max.x, min.y, 0.0, 0.0), // tr
            vec4(min.x, min.y, 0.0, 0.0), // tl
            vec4(min.x, max.y, 0.0, 0.0), // bl
            vec4(max.x, max.y, 0.0, 0.0), // br
        ],
    )
}
