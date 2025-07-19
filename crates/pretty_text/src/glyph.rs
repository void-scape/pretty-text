//! Runs systems for glyph generation and positioning.
//!
//! See [`GlyphMeshPlugin`].

use bevy::{
    ecs::entity::EntityHashSet,
    platform::collections::HashMap,
    prelude::*,
    render::view::{RenderLayers, VisibilitySystems},
    sprite::Anchor,
    text::{ComputedTextBlock, PositionedGlyph, TextBounds, TextLayoutInfo, Update2dText},
    window::PrimaryWindow,
};

use crate::PrettyText;

/// Core systems related to glyph processing.
#[derive(Debug, SystemSet, PartialEq, Eq, Hash, Clone)]
pub enum GlyphSystems {
    /// Construction of [`Glyph`] entities derived from a text hierarchy's
    /// [`TextLayoutInfo`].
    ///
    /// Runs in the [`PostUpdate`] schedule before `GlyphSystems::PropagateMaterial`.
    Construct,

    /// Apply [text materials](crate::material) to glyph entities.
    ///
    /// Runs in the [`PostUpdate`] schedule after `GlyphSystems::Construct`.
    PropagateMaterial,

    /// Propagate glyph transforms and calculate positions using [`GlyphOrigin`]
    /// and [`GlyphOffset`].
    ///
    /// Runs in the [`FixedUpdate`] schedule.
    ///
    /// Custom [ECS driven effects](crate::dynamic_effects) should update the `GlyphOffset` in
    /// `FixedUpdate` before this set.
    Position,
}

/// Runs systems to generate and position [`Glyph`]s from [`Text2d`] entities.
///
/// [`Glyph`] construction occurs in the [`GlyphSystems::Construct`] system set
/// within the [`PostUpdate`] schedule. `Glyph` positioning happens in the
/// [`GlyphSystems::Position`] set within the [`FixedUpdate`] schedule.
#[derive(Debug)]
pub struct GlyphMeshPlugin;

impl Plugin for GlyphMeshPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<GlyphCache>()
            .init_resource::<ShouldReposition>()
            .add_systems(
                PostUpdate,
                (
                    (
                        glyphify_text2d.in_set(GlyphSystems::Construct),
                        #[cfg(not(test))]
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
                    .in_set(GlyphSystems::Position),
            )
            .configure_sets(
                PostUpdate,
                (
                    GlyphSystems::Construct.after(Update2dText),
                    GlyphSystems::PropagateMaterial.after(GlyphSystems::Construct),
                ),
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

/// Tracks related glyph entities.
///
/// `Glyphs` points to free-standing [`Glyph`] entities. This relationship thinly
/// wraps the [`PositionedGlyph`] data from [`TextLayoutInfo`].
#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub struct Glyphs(Vec<Entity>);

/// Stores the [text root entity](Glyphs).
///
/// This entity stores the necessary [`Glyph`] data for rendering text.
#[derive(Debug, Clone, Copy, Component, Reflect)]
#[relationship(relationship_target = Glyphs)]
pub struct GlyphOf(Entity);

impl GlyphOf {
    /// The text root entity.
    pub fn root(&self) -> Entity {
        self.0
    }
}

/// Wrapper around [`PositionedGlyph`].
///
/// `Glyph` is related to a [`Text2d`] entity with [`GlyphOf`]. The specific
/// text span entity is stored in [`GlyphSpanEntity`].
///
/// Each glyph entity is positioned in the world relative to its root and
/// rendered with a mesh.
///
/// See [`GlyphOffset`] and [`GlyphOrigin`] for applying position related
/// effects.
///
/// See [`crate::material`] for applying shader effects.
///
/// The glyph's mesh has texture atlas uv data packed into its vertices for
/// sampling from a glyph atlas in a shader.
#[derive(Debug, Clone, Component, Reflect)]
#[require(GlyphOrigin, GlyphOffset)]
pub struct Glyph(pub PositionedGlyph);

/// Stores the text span entity for a [`Glyph`].
///
/// Accessing the components on a text span entity are useful for applying
/// [ECS driven effects](crate::dynamic_effects) and propagating mesh
/// [materials](bevy::sprite::Material2d).
#[derive(Debug, Clone, Copy, Component, Reflect)]
pub struct GlyphSpanEntity(pub Entity);

/// Cached glyph atlas handle.
///
/// Each text span in a text hierarchy can have a different [`TextFont`], and
/// therefore, a different glyph atlas.
///
/// The glyph atlas must be supplied to the [`Glyph`] entity meshes for
/// rendering during [material propagation](crate::material::set_material_atlas).
/// Since every [`Glyph`] entity points to its span with [`GlyphSpanEntity`], the
/// glyph atlas is cached on the span entity in `SpanAtlasImage`.
#[derive(Debug, Clone, Component, Reflect)]
pub struct SpanAtlasImage(pub Handle<Image>);

// Glyphs are hashed so that less meshes are allocated, although I don't know
// if this is necessary ¯\_(ツ)_/¯
//
// This is also never cleared!
#[derive(Default, Deref, DerefMut, Resource)]
struct GlyphCache(HashMap<GlyphHash, Handle<Mesh>>);

#[derive(PartialEq, Eq, Hash)]
struct GlyphHash {
    size: UVec2,

    // weak handles
    texture: AssetId<Image>,
    texture_atlas: AssetId<TextureAtlasLayout>,

    glyph_index: usize,
    offset: IVec2,

    color: [u8; 4],
}

fn glyphify_text2d(
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
        (Changed<TextLayoutInfo>, With<PrettyText>, With<Text2d>),
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
                    // insert `PrettyText` to make sure that this span receives a material
                    .insert((PrettyText, SpanAtlasImage(glyph.atlas_info.texture.clone())));
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

#[cfg(not(test))]
fn insert_glyph_mesh(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut glyph_cache: ResMut<GlyphCache>,
    atlases: Res<Assets<TextureAtlasLayout>>,
    glyphs: Query<(Entity, &Glyph, &GlyphSpanEntity), Changed<Glyph>>,
    text_color: Query<&TextColor>,
) -> Result {
    use bevy::{
        asset::RenderAssetUsages,
        render::mesh::{Indices, PrimitiveTopology},
        text::GlyphAtlasLocation,
    };

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

/// The stable position for a [`Glyph`] entity.
///
/// This is calculated with `Bevy`'s built-in text layout.
///
/// ECS driven effects can accumulate position offset in [`GlyphOffset`] during
/// the [`FixedUpdate`] schedule.
#[derive(Debug, Default, Clone, Deref, Component, Reflect)]
pub struct GlyphOrigin(Vec3);

/// An accumulated position offset relative to the [`GlyphOrigin`].
///
/// The accumulated offset is cleared and applied to a [`Glyph`] during the
/// [`GlyphSystems::Position`] set in [`FixedUpdate`] schedule.
///
/// # Example
///
/// ```
/// # use bevy::prelude::*;
/// # use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
/// # use pretty_text::glyph::{GlyphOffset, GlyphSpanEntity, GlyphSystems};
/// # use pretty_text::PrettyText;
/// #
/// // mark a glyph for wobbling.
/// #[derive(Component)]
/// struct ComputeWobble;
///
/// fn wobble(
///     time: Res<Time>,
///     mut glyphs: Query<&mut GlyphOffset, With<ComputeWobble>>,
/// ) {
///     let intensity = 1f32;
///     let radius = 5f32;
///
///     for (i, mut offset) in glyphs.iter_mut().enumerate() {
///         let i = i as f32;
///         let time_factor = time.elapsed_secs() * intensity * 8.0;
///
///         // random math stuff
///         let x = time_factor.sin() * (time_factor * 1.3 + i * 2.0).cos();
///         let y = time_factor.cos() * (time_factor * 1.7 + i * 3.0).sin();
///
///         // apply some motion!
///         offset.0 += (Vec2::new(x, y) * radius * time.delta_secs() * 60.0).extend(0.);
///     }
/// }
/// ```
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

#[cfg(test)]
mod test {
    use bevy::prelude::*;

    use crate::PrettyText;
    use crate::test::{prepare_app, roots, run};

    use super::Glyph;

    #[test]
    fn glyph_entities() {
        roots().for_each(|(str, root)| test_str(str, root));
    }

    fn test_str(str: &'static str, root: impl Bundle) {
        let mut app = prepare_app();
        let entity = app.world_mut().spawn((PrettyText, root)).id();
        app.world_mut().run_schedule(PostUpdate);
        app.world_mut().flush();

        run(&mut app, |glyphs: Query<&Glyph>| {
            assert_eq!(glyphs.iter().len(), str.chars().count());
        });

        app.world_mut().entity_mut(entity).despawn();

        run(&mut app, |glyphs: Query<&Glyph>| {
            assert!(glyphs.is_empty());
        });
    }
}
