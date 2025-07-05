use bevy::{
    asset::RenderAssetUsages,
    ecs::entity::EntityHashSet,
    platform::collections::HashMap,
    prelude::*,
    render::{
        mesh::{Indices, PrimitiveTopology},
        view::{RenderLayers, VisibilitySystems},
    },
    sprite::Anchor,
    text::{ComputedTextBlock, GlyphAtlasLocation, PositionedGlyph, TextBounds, TextLayoutInfo},
    window::PrimaryWindow,
};

use crate::{PrettyText, PrettyTextSystems};

pub struct GlyphMeshPlugin;

impl Plugin for GlyphMeshPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<GlyphCache>()
            .init_resource::<ShouldReposition>()
            .add_systems(
                PostUpdate,
                (
                    (
                        gliphify_text2d.in_set(PrettyTextSystems::GlyphConstruct),
                        insert_glyph_mesh,
                    )
                        .chain(),
                    hide_builtin_text
                        .in_set(VisibilitySystems::CheckVisibility)
                        .after(bevy::render::view::check_visibility),
                ),
            )
            .add_systems(
                FixedUpdate,
                (should_reposition, glyph_transform_propagate, offset_glyphs)
                    .chain()
                    .in_set(PrettyTextSystems::GlyphPosition),
            );

        app.register_type::<Glyph>()
            .register_type::<Glyphs>()
            .register_type::<GlyphOf>()
            .register_type::<GlyphSpanEntity>()
            .register_type::<GlyphOrigin>()
            .register_type::<GlyphOffset>()
            .register_type::<SpanAtlasImage>();
    }
}

#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub struct Glyphs(Vec<Entity>);

#[derive(Debug, Clone, Copy, Component, Reflect)]
#[relationship(relationship_target = Glyphs)]
pub struct GlyphOf(pub Entity);

#[derive(Debug, Clone, Component, Reflect)]
#[require(GlyphOrigin, GlyphOffset)]
pub struct Glyph(pub PositionedGlyph);

#[derive(Debug, Clone, Copy, Component, Reflect)]
pub struct GlyphSpanEntity(pub Entity);

#[derive(Debug, Clone, Component, Reflect)]
pub struct SpanAtlasImage(pub Handle<Image>);

#[derive(Default, Deref, DerefMut, Resource)]
struct GlyphCache(HashMap<GlyphHash, Handle<Mesh>>);

#[derive(PartialEq, Eq, Hash)]
struct GlyphHash {
    size: UVec2,

    texture: AssetId<Image>,
    texture_atlas: AssetId<TextureAtlasLayout>,

    glyph_index: usize,
    offset: IVec2,

    color: [u8; 4],
}

fn gliphify_text2d(
    mut commands: Commands,
    mut text2d: Query<
        (
            Entity,
            &GlobalTransform,
            &ComputedTextBlock,
            &TextLayoutInfo,
            &TextBounds,
            &Anchor,
            Option<&RenderLayers>,
        ),
        (Changed<TextLayoutInfo>, With<PrettyText>),
    >,
    windows: Query<&Window, With<PrimaryWindow>>,
) -> Result {
    let scale_factor = windows
        .single()
        .map(|window| window.resolution.scale_factor())
        .unwrap_or(1.0);
    let scaling = GlobalTransform::from_scale(Vec2::splat(scale_factor.recip()).extend(1.));

    for (entity, gt, computed, layout, text_bounds, anchor, layers) in text2d.iter_mut() {
        commands.entity(entity).despawn_related::<Glyphs>();

        let mut processed_spans = Vec::new();
        let text_entities = computed.entities();
        let layers = layers.cloned().unwrap_or_default();

        for (i, glyph) in layout.glyphs.iter().enumerate() {
            if !processed_spans.contains(&glyph.span_index) {
                processed_spans.push(glyph.span_index);
                commands
                    .entity(text_entities[glyph.span_index].entity)
                    .insert(SpanAtlasImage(glyph.atlas_info.texture.clone()));
            }

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

            commands.spawn((
                Visibility::Visible,
                GlyphOf(entity),
                Glyph(glyph.clone()),
                GlyphSpanEntity(text_entities[glyph.span_index].entity),
                transform.compute_transform(),
                transform,
                layers.clone(),
            ));
        }
    }

    Ok(())
}

fn insert_glyph_mesh(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut glyph_cache: ResMut<GlyphCache>,
    atlases: Res<Assets<TextureAtlasLayout>>,
    glyphs: Query<(Entity, &Glyph, &GlyphSpanEntity), Changed<Glyph>>,
    text_color: Query<&TextColor>,
) -> Result {
    for (entity, glyph, span_entity) in glyphs.iter() {
        let color = text_color
            .get(span_entity.0)
            .map_err(|_| "invalid `Text2d` structure: failed to fetch glyph color")?
            .0;

        // TODO: will this ever fail?
        let atlas = atlases
            .get(&glyph.0.atlas_info.texture_atlas)
            .ok_or("failed to turn `Text2d` into glyphs: font atlas has not loaded yet")?;

        let mesh = glyph_cache
            .entry(GlyphHash {
                size: (glyph.0.size * 1_000.0).as_uvec2(),
                texture: AssetId::from(&glyph.0.atlas_info.texture),
                texture_atlas: AssetId::from(&glyph.0.atlas_info.texture_atlas),
                glyph_index: glyph.0.atlas_info.location.glyph_index,
                offset: glyph.0.atlas_info.location.offset,
                color: color.to_linear().to_u8_array(),
            })
            .or_insert_with(|| {
                meshes.add(glyph_mesh(
                    glyph.0.size.x,
                    glyph.0.size.y,
                    atlas,
                    &glyph.0.atlas_info.location,
                    color,
                ))
            })
            .clone();
        commands.entity(entity).insert(Mesh2d(mesh));
    }

    Ok(())
}

#[derive(Default, Resource)]
struct ShouldReposition(EntityHashSet);

// TODO: is this necessary?
fn should_reposition(
    mut should_reposition: ResMut<ShouldReposition>,
    glyphs: Query<&GlyphOf, Changed<Glyph>>,
) {
    should_reposition.0.clear();
    should_reposition
        .0
        .extend(glyphs.iter().map(|glyph_of| glyph_of.0));
}

// Spawning glyphs as children of `Text2d` will cause the layout to recompute ... looping
// infinitely!
fn glyph_transform_propagate(
    should_reposition: Res<ShouldReposition>,
    mut origins: Query<(&mut Transform, &mut GlyphOrigin, &Glyph), With<GlyphOf>>,
    roots: Query<
        (
            Entity,
            Ref<GlobalTransform>,
            &Glyphs,
            &TextLayoutInfo,
            &TextBounds,
            &Anchor,
        ),
        Without<GlyphOf>,
    >,
    windows: Query<&Window, With<PrimaryWindow>>,
) {
    let scale_factor = windows
        .single()
        .map(|window| window.resolution.scale_factor())
        .unwrap_or(1.0);
    let scaling = GlobalTransform::from_scale(Vec2::splat(scale_factor.recip()).extend(1.));

    for (entity, gt, glyphs, layout, text_bounds, anchor) in roots.iter() {
        if !gt.is_changed() && !should_reposition.0.contains(&entity) {
            continue;
        }

        let size = Vec2::new(
            text_bounds.width.unwrap_or(layout.size.x),
            text_bounds.height.unwrap_or(layout.size.y),
        );
        let bottom_left = -(anchor.as_vec() + 0.5) * size + (size.y - layout.size.y) * Vec2::Y;

        let mut iter = origins.iter_many_mut(glyphs.iter());
        let mut i = 0;
        while let Some((mut transform, mut origin, glyph)) = iter.fetch_next() {
            // TODO: z ordering?
            *transform = (*gt
                * GlobalTransform::from_translation(bottom_left.extend(0.))
                * scaling
                * GlobalTransform::from_translation(glyph.0.position.extend(i as f32 * 0.001)))
            .compute_transform();
            origin.0 = transform.translation;
            i += 1;
        }
    }
}

#[derive(Debug, Default, Clone, Deref, Component, Reflect)]
pub struct GlyphOrigin(Vec3);

#[derive(Debug, Default, Clone, Deref, DerefMut, Component, Reflect)]
pub struct GlyphOffset(pub Vec3);

fn offset_glyphs(mut glyphs: Query<(&mut Transform, &GlyphOrigin, &mut GlyphOffset)>) {
    for (mut transform, origin, mut offset) in glyphs.iter_mut() {
        transform.translation = origin.0 + offset.0;
        offset.0 = Vec3::default();
    }
}

// `PrettyText` entities *must* be hidden otherwise text will be rendered here and in the default Text2d
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
    color: Color,
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
        vec![color.to_linear().to_f32_array(); 4],
    )
    .with_inserted_attribute(
        Mesh::ATTRIBUTE_NORMAL,
        // atlas uvs
        vec![
            vec3(max.x, min.y, 0.0), // tr
            vec3(min.x, min.y, 0.0), // tl
            vec3(min.x, max.y, 0.0), // bl
            vec3(max.x, max.y, 0.0), // br
        ],
    )
}
