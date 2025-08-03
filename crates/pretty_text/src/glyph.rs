//! Runs systems for glyph generation and positioning.
//!
//! See [`GlyphPlugin`].

use bevy::{
    ecs::system::SystemParam,
    prelude::*,
    render::view::VisibilitySystems,
    text::{ComputedTextBlock, PositionedGlyph, TextLayoutInfo, Update2dText},
    ui::UiSystem,
};

use crate::{
    PrettyText,
    effects::material::{Materials, PropogateMaterial},
    style::PrettyStyleSet,
};

const DEFAULT_FONT_SIZE: f32 = 20f32;

/// Core systems related to glyph processing.
///
/// Runs in the [`PostUpdate`] schedule.
#[derive(Debug, SystemSet, PartialEq, Eq, Hash, Clone)]
pub enum GlyphSystems {
    /// Construction of [`Glyph`] entities derived from a text hierarchy's
    /// [`TextLayoutInfo`].
    ///
    /// Runs before [`GlyphSystems::PropagateMaterial`].
    Construct,

    /// Apply [text materials](crate::material) to glyph entities.
    ///
    /// Runs after [`GlyphSystems::Construct`].
    PropagateMaterial,

    /// Propogate [`InheritedVisibility`] to glyph entities.
    Visibility,

    /// Process glyph transformations.
    Transform,
}

/// Runs systems to generate and position [`Glyph`]s for [`Text`] and [`Text2d`] entities.
///
/// See [`GlyphSystems`] for scheduling details.
#[derive(Debug)]
pub struct GlyphPlugin;

impl Plugin for GlyphPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PostUpdate,
            (
                (glyphify_text, glyph_scale)
                    .chain()
                    .in_set(GlyphSystems::Construct),
                (
                    propagate_visibility
                        .after(VisibilitySystems::VisibilityPropagate)
                        .before(VisibilitySystems::CheckVisibility),
                    hide_builtin_text
                        .in_set(VisibilitySystems::CheckVisibility)
                        .after(bevy::render::view::check_visibility),
                )
                    .chain()
                    .in_set(GlyphSystems::Visibility),
                glyph_transformations.in_set(GlyphSystems::Transform),
            ),
        )
        // `extract_glyphs` in `ui_pipeline` needs access to the glyph offset
        // during the extraction schedule.
        .add_systems(First, (clear_glyph_transformations, unhide_builtin_text))
        .configure_sets(
            PostUpdate,
            (
                GlyphSystems::Construct
                    .after(Update2dText)
                    .after(UiSystem::Stack),
                GlyphSystems::PropagateMaterial
                    .after(GlyphSystems::Construct)
                    .after(PrettyStyleSet),
                GlyphSystems::Transform.before(TransformSystem::TransformPropagate),
            ),
        );

        app.register_type::<Glyph>()
            .register_type::<Glyphs>()
            .register_type::<GlyphOf>()
            .register_type::<SpanGlyphs>()
            .register_type::<SpanGlyphOf>()
            .register_type::<GlyphSpan>()
            .register_type::<GlyphPosition>()
            .register_type::<GlyphRotation>()
            .register_type::<GlyphScale>()
            .register_type::<LocalGlyphScale>()
            .register_type::<SpanAtlasImage>();
    }
}

/// Tracks related glyph entities.
///
/// `Glyphs` points to free-standing [`Glyph`] entities. This relationship is an
/// ECS wrapper around the [`PositionedGlyph`] data stored in [`TextLayoutInfo`].
#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub struct Glyphs(Vec<Entity>);

/// Stores the [text root entity](Glyphs).
///
/// This entity stores the necessary [`Glyph`] data for rendering text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[relationship(relationship_target = Glyphs)]
pub struct GlyphOf(Entity);

impl GlyphOf {
    /// The text root entity.
    pub fn root(&self) -> Entity {
        self.0
    }
}

#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = SpanGlyphOf)]
pub struct SpanGlyphs(Vec<Entity>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[relationship(relationship_target = SpanGlyphs)]
pub struct SpanGlyphOf(Entity);

impl SpanGlyphOf {
    pub fn span(&self) -> Entity {
        self.0
    }
}

/// Wrapper around [`PositionedGlyph`].
///
/// `Glyph` is related to a [`Text2d`] entity with [`GlyphOf`]. The specific
/// text span entity is stored in [`GlyphSpan`].
///
/// Each glyph entity is positioned in the world relative to its root and
/// rendered with a mesh.
///
/// See [`dynamic_effects`](crate::dynamic_effects) for writing custom effects.
///
/// The glyph's mesh has texture atlas uv data packed into its vertices for
/// sampling from a glyph atlas in a shader.
#[derive(Debug, Clone, Component, Deref, Reflect)]
#[require(Transform, GlyphPosition, LocalGlyphScale, GlyphRotation)]
pub struct Glyph(pub PositionedGlyph);

#[derive(Debug, Default, Clone, PartialEq, Deref, Component, Reflect)]
#[component(immutable)]
pub struct GlyphInstance(pub usize);

/// An accumulated position offset.
///
/// The accumulated offset is cleared and applied to a [`Glyph`] during the
/// [`GlyphSystems::Transform`] set in [`PostUpdate`] schedule.
#[derive(Debug, Default, Clone, PartialEq, Deref, DerefMut, Component, Reflect)]
pub struct GlyphPosition(pub Vec3);

/// The product of the root [`GlobalTransform::scale`] and [`TextFont::font_size`].
///
/// [`GlyphScale`] is an immutable component derived from the text hierarchy.
/// [`LocalGlyphScale`] is a scale modifier applied to the rendered glyph.
///
/// [Dynamic](crate::dynamic_effects) and [material](crate::material) effects
/// should use this value to scale their parameters uniformly across all [`Glyph`]
/// sizes.
#[derive(Debug, Default, Clone, Copy, PartialEq, Deref, Component, Reflect)]
#[component(immutable)]
pub struct GlyphScale(pub Vec2);

fn glyph_scale(
    mut commands: Commands,
    spans: Query<
        (Entity, &GlobalTransform, &TextFont),
        Or<(Changed<GlobalTransform>, Changed<TextFont>)>,
    >,
    mut glyphs: Query<(Entity, &GlyphSpan), (With<GlyphScale>, With<Glyph>)>,
) {
    for (entity, gt, font) in spans.iter() {
        for (entity, _) in glyphs.iter_mut().filter(|(_, span)| span.0 == entity) {
            commands.entity(entity).insert(GlyphScale(
                gt.scale().xy() * font.font_size / DEFAULT_FONT_SIZE,
            ));
        }
    }
}

/// An accumulated scale modifier.
///
/// The accumulated scale is cleared and applied to a [`Glyph`] during the
/// [`GlyphSystems::Transform`] set in [`PostUpdate`] schedule.
#[derive(Debug, Default, Clone, Copy, PartialEq, Deref, Component, Reflect)]
pub struct LocalGlyphScale(pub Vec2);

#[derive(Default, Component)]
struct RetainedLocalGlyphScale(Vec2);

/// An accumulated rotation in radians.
///
/// The accumulated rotation is cleared and applied to a [`Glyph`] during the
/// [`GlyphSystems::Transform`] set in [`PostUpdate`] schedule.
#[derive(Debug, Default, Clone, Copy, PartialEq, Deref, Component, Reflect)]
pub struct GlyphRotation(pub f32);

fn glyph_transformations(
    mut glyphs: Query<(
        &mut Transform,
        &GlyphPosition,
        &RetainedLocalGlyphScale,
        &LocalGlyphScale,
        &GlyphRotation,
    )>,
) {
    for (mut transform, position, retained_scale, scale, rotation) in glyphs.iter_mut() {
        if transform.translation != position.0 {
            transform.translation = position.0;
        }

        let s = (retained_scale.0 + scale.0).extend(1f32);
        if transform.scale != s {
            transform.scale = s;
        }

        let r = Quat::from_rotation_z(rotation.0);
        if transform.rotation != r {
            transform.rotation = r;
        }
    }
}

fn clear_glyph_transformations(
    mut offsets: Query<(&mut GlyphPosition, &mut LocalGlyphScale, &mut GlyphRotation)>,
) {
    for (mut offset, mut scale, mut rotation) in offsets.iter_mut() {
        offset.0 = Vec3::default();
        scale.0 = Vec2::default();
        rotation.0 = 0f32;
    }
}

/// Stores the text span entity for a [`Glyph`].
///
/// Accessing the components on a text span entity are useful for applying
/// [ECS driven effects](crate::dynamic_effects) and propagating mesh
/// [materials](bevy::sprite::Material2d).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub struct GlyphSpan(pub Entity);

impl ContainsEntity for GlyphSpan {
    fn entity(&self) -> Entity {
        self.0
    }
}

/// Stores the text root entity for a [`Glyph`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub struct GlyphRoot(pub Entity);

/// Cached glyph atlas handle.
///
/// Each text span in a text hierarchy can have a different [`TextFont`], and
/// therefore, a different glyph atlas.
///
/// The glyph atlas must be supplied to the [`Glyph`] entity meshes for
/// rendering during [material propagation](crate::material::set_material_atlas).
/// Since every [`Glyph`] entity points to its span with [`GlyphSpan`], the
/// glyph atlas is cached on the span entity in `SpanAtlasImage`.
#[derive(Debug, Clone, PartialEq, Eq, Component, Reflect)]
#[require(Materials)]
pub struct SpanAtlasImage(pub Handle<Image>);

/// Utility for reading the text data pointed to by a [`Glyph`] entity.
#[derive(Debug, SystemParam)]
pub struct GlyphReader<'w, 's> {
    computed: Query<'w, 's, &'static ComputedTextBlock>,
    glyphs: Query<'w, 's, (&'static Glyph, &'static GlyphOf)>,
}

impl<'w, 's> GlyphReader<'w, 's> {
    /// Retrieve the text data pointed to by a `glyph`.
    pub fn read(&self, glyph: Entity) -> Result<&str> {
        Ok(self.glyphs.get(glyph).map(|(glyph, glyph_of)| {
            self.computed.get(glyph_of.root()).map(|computed| {
                let text = &computed.buffer().lines[glyph.0.line_index].text();
                &text[glyph.0.byte_index..glyph.0.byte_index + glyph.0.byte_length]
            })
        })??)
    }
}

fn glyphify_text(
    mut commands: Commands,
    mut text: Query<
        (
            Entity,
            &GlobalTransform,
            &ComputedTextBlock,
            &TextLayoutInfo,
        ),
        (Changed<TextLayoutInfo>, With<PrettyText>),
    >,
    fonts: Query<&TextFont>,
) -> Result {
    for (entity, gt, computed, layout) in text.iter_mut() {
        commands
            .entity(entity)
            .despawn_related::<Glyphs>()
            .insert(RetainedInheritedVisibility::default());

        let text_entities = computed.entities();
        let mut processed_spans = Vec::with_capacity(text_entities.len());

        for (i, glyph) in layout.glyphs.iter().enumerate() {
            let span_entity = text_entities[glyph.span_index].entity;
            if !processed_spans.contains(&glyph.span_index) {
                processed_spans.push(glyph.span_index);
                commands.entity(span_entity).insert((
                    PropogateMaterial,
                    SpanAtlasImage(glyph.atlas_info.texture.clone()),
                ));
            }

            let font = fonts
                .get(text_entities[glyph.span_index].entity)
                .map_err(|_| "Invalid text hierarchy: `TextSpan` has no `TextFont`")?;

            // the position of glyphs is calculated by `extract_glyphs` in `ui_pipeline`.
            commands.spawn((
                Visibility::Inherited,
                GlyphOf(entity),
                SpanGlyphOf(span_entity),
                Glyph(glyph.clone()),
                GlyphInstance(i),
                GlyphSpan(text_entities[glyph.span_index].entity),
                GlyphScale(gt.scale().xy() * font.font_size / DEFAULT_FONT_SIZE),
                RetainedLocalGlyphScale(gt.scale().xy()),
                GlyphRoot(entity),
            ));
        }
    }

    Ok(())
}

// `Glyph`s are free-standing entities, so the visibility of the root needs to be propagated.
fn propagate_visibility(
    roots: Query<(&InheritedVisibility, &Glyphs)>,
    mut glyph_visibility: Query<(&mut InheritedVisibility, &Visibility), Without<Glyphs>>,
) {
    for (root_inherited, glyphs) in roots.iter() {
        let mut iter = glyph_visibility.iter_many_mut(glyphs.iter());
        while let Some((mut inherited_visibility, glyph_visibility)) = iter.fetch_next() {
            let inherit = match glyph_visibility {
                Visibility::Visible => InheritedVisibility::VISIBLE,
                Visibility::Hidden => InheritedVisibility::HIDDEN,
                Visibility::Inherited => *root_inherited,
            };

            if *inherited_visibility != inherit {
                *inherited_visibility = inherit;
            }
        }
    }
}

// This is not a very good solution.
#[derive(Default, Component)]
struct RetainedInheritedVisibility(InheritedVisibility);

// `PrettyText` text entities *must* be hidden otherwise text will be rendered here and
// in the default text pipelines.
fn hide_builtin_text(
    mut text2d_vis: Query<&mut ViewVisibility, (With<PrettyText>, With<Text2d>, Without<Text>)>,
    mut text_vis: Query<
        (
            &mut RetainedInheritedVisibility,
            &mut InheritedVisibility,
            &mut ViewVisibility,
        ),
        (With<PrettyText>, With<Text>),
    >,
) {
    for mut vis in text2d_vis.iter_mut() {
        *vis = ViewVisibility::HIDDEN;
    }

    // `extract_text_sections` checks for `InheritedVisibility`, not `ViewVisibility`
    for (mut retained, mut inherited, mut view) in text_vis.iter_mut() {
        retained.0 = *inherited;
        *inherited = InheritedVisibility::HIDDEN;
        *view = ViewVisibility::HIDDEN;
    }
}

fn unhide_builtin_text(
    mut text_vis: Query<
        (&RetainedInheritedVisibility, &mut InheritedVisibility),
        (With<PrettyText>, With<Text>),
    >,
) {
    for (retained, mut inherited) in text_vis.iter_mut() {
        *inherited = retained.0;
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

    // This test currently fails for wide glyphs due to an upstream issue.

    // use bevy::prelude::*;
    //
    // use crate::glyph::Glyphs;
    // use crate::test::{prepare_app, run, run_tests};
    //
    // use super::GlyphReader;
    //
    // #[test]
    // fn glyph_reader() {
    //     run_tests(prepare_app, |app, _, str| {
    //         app.world_mut().run_schedule(PostUpdate);
    //         app.world_mut().flush();
    //         run(app, move |reader: GlyphReader, root: Single<&Glyphs>| {
    //             let repro = root
    //                 .iter()
    //                 .map(|glyph| reader.read(glyph).unwrap())
    //                 .collect::<String>();
    //
    //             assert_eq!(
    //                 repro.chars().count(),
    //                 str.chars().count(),
    //                 "failed with: \"{}\", read as \"{}\"",
    //                 str,
    //                 repro
    //             );
    //         });
    //     });
    // }
}
