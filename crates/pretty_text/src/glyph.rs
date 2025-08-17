//! Runs systems for glyph generation and positioning.
//!
//! See [`GlyphPlugin`].

use bevy::{
    ecs::system::SystemParam,
    prelude::*,
    render::view::VisibilitySystems,
    text::{ComputedTextBlock, PositionedGlyph, TextLayoutInfo, update_text2d_layout},
    ui::UiSystem,
};

use crate::PrettyText;

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
                ),
                glyph_vertices.in_set(GlyphSystems::Transform),
            ),
        )
        .add_systems(First, unhide_builtin_text)
        .configure_sets(
            PostUpdate,
            (
                GlyphSystems::Construct
                    .after(update_text2d_layout)
                    .after(UiSystem::Stack)
                    .before(VisibilitySystems::VisibilityPropagate),
                GlyphSystems::Transform.after(GlyphSystems::Construct),
            ),
        );

        app.register_type::<Glyph>()
            .register_type::<Glyphs>()
            .register_type::<GlyphOf>()
            .register_type::<SpanGlyphs>()
            .register_type::<SpanGlyphOf>()
            .register_type::<GlyphSpan>()
            .register_type::<GlyphIndex>()
            .register_type::<GlyphRoot>()
            .register_type::<GlyphVertices>()
            .register_type::<SpanLength>()
            .register_type::<GlyphScale>();
    }
}

#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub struct Glyphs(Vec<Entity>);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub struct SpanLength(pub usize);

#[derive(Debug, Clone, Component, Deref, Reflect)]
#[require(Transform, GlyphVertices, GlyphTransforms)]
pub struct Glyph(pub PositionedGlyph);

#[derive(Debug, Default, Clone, PartialEq, Deref, Component, Reflect)]
#[component(immutable)]
pub struct GlyphIndex(pub usize);

#[derive(Debug, Default, Clone, Copy, PartialEq, Deref, DerefMut, Component, Reflect)]
pub struct GlyphVertices(pub [GlyphVertex; 4]);

impl GlyphVertices {
    #[inline]
    pub fn splat(vertex: GlyphVertex) -> Self {
        Self([vertex; 4])
    }

    #[inline]
    pub fn mask(&mut self, mask: impl Into<VertexMask>) -> MaskedVertices<'_> {
        MaskedVertices(self, mask.into())
    }

    pub fn mask_clear(mut self, mask: impl Into<VertexMask>) -> Self {
        self.mask(mask).clear();
        self
    }
}

#[derive(Debug)]
pub struct MaskedVertices<'a>(&'a mut GlyphVertices, VertexMask);

impl MaskedVertices<'_> {
    pub fn clear(self) -> Self {
        self.0
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| (((1 << i) & self.1.0) == 0).then_some(v))
            .for_each(|v| v.clear());
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &GlyphVertex> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| (((1 << i) & self.1.0) != 0).then_some(v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut GlyphVertex> {
        self.0
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| (((1 << i) & self.1.0) != 0).then_some(v))
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Reflect)]
pub struct GlyphVertex {
    pub translation: Vec2,
    pub rotation: f32,
    pub scale: Vec2,
}

impl GlyphVertex {
    #[inline]
    pub fn from_translation(translation: Vec2) -> Self {
        Self {
            translation,
            ..Default::default()
        }
    }

    #[inline]
    pub fn from_rotation(rotation: f32) -> Self {
        Self {
            rotation,
            ..Default::default()
        }
    }

    #[inline]
    pub fn from_scale(scale: Vec2) -> Self {
        Self {
            scale,
            ..Default::default()
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        *self = Self::default();
    }

    pub fn compute_transform(&self) -> Transform {
        Transform::from_translation(self.translation.extend(0.0))
            .with_rotation(Quat::from_rotation_z(self.rotation))
            .with_scale((self.scale + Vec2::ONE).extend(1.0))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub struct VertexMask(u8);

impl Default for VertexMask {
    #[inline]
    fn default() -> Self {
        Self::ALL
    }
}

impl VertexMask {
    pub const ALL: Self = Self(0b1111);
    pub const TL: Self = Self(0b0001);
    pub const TR: Self = Self(0b0010);
    pub const BL: Self = Self(0b0100);
    pub const BR: Self = Self(0b1000);
}

impl std::ops::BitOr for VertexMask {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl std::ops::BitAnd for VertexMask {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl Into<VertexMask> for &VertexMask {
    fn into(self) -> VertexMask {
        *self
    }
}

/// The product of the root [`GlobalTransform::scale`] and [`TextFont::font_size`].
///
/// [`GlyphScale`] is an immutable component derived from the text hierarchy.
///
/// Effects should use this value to scale their parameters uniformly across
/// all [`Glyph`] sizes.
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

#[derive(Debug, Default, Clone, Copy, Deref, DerefMut, PartialEq, Component)]
pub(crate) struct GlyphTransforms(pub [Transform; 4]);

fn glyph_vertices(mut glyphs: Query<(&mut GlyphVertices, &mut GlyphTransforms)>) {
    for (mut vertices, mut transforms) in glyphs.iter_mut() {
        transforms[0] = vertices[0].compute_transform();
        transforms[1] = vertices[1].compute_transform();
        transforms[2] = vertices[2].compute_transform();
        transforms[3] = vertices[3].compute_transform();
        vertices[0].clear();
        vertices[1].clear();
        vertices[2].clear();
        vertices[3].clear();
    }
}

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
        for (i, glyph) in layout.glyphs.iter().enumerate() {
            let span_entity = text_entities[glyph.span_index].entity;

            let font = fonts
                .get(text_entities[glyph.span_index].entity)
                .map_err(|_| "Invalid text hierarchy: `TextSpan` has no `TextFont`")?;

            commands.spawn((
                Visibility::Inherited,
                GlyphOf(entity),
                SpanGlyphOf(span_entity),
                Glyph(glyph.clone()),
                GlyphIndex(i),
                GlyphSpan(text_entities[glyph.span_index].entity),
                GlyphScale(gt.scale().xy() * font.font_size / DEFAULT_FONT_SIZE),
                GlyphRoot(entity),
                SpanLength(layout.glyphs.len()),
            ));
        }
    }

    Ok(())
}

// `Glyph`s are free-standing entities, so the visibility of the root needs to be propagated.
pub(crate) fn propagate_visibility(
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
