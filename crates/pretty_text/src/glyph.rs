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
    Construct,
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
            ),
        )
        .add_systems(First, (unhide_builtin_text, clear_vertices))
        .configure_sets(
            PostUpdate,
            GlyphSystems::Construct
                .after(update_text2d_layout)
                .after(UiSystem::Stack)
                .before(VisibilitySystems::VisibilityPropagate),
        );

        app.register_type::<Glyph>()
            .register_type::<Glyphs>()
            .register_type::<GlyphOf>()
            .register_type::<SpanGlyphs>()
            .register_type::<SpanGlyphOf>()
            .register_type::<GlyphCount>()
            .register_type::<GlyphIndex>()
            .register_type::<GlyphVertices>()
            .register_type::<GlyphScale>();
    }
}

/// Tracks which [`Glyph`] entities are glyphs of this entity.
///
/// Text roots with the [`PrettyText`] marker will store their [`Glyphs`].
#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = GlyphOf, linked_spawn)]
pub struct Glyphs(Vec<Entity>);

/// Tracks the [`Glyphs`] entity in this component.
///
/// All [`Glyph`] entities will store the text root entity in [`GlyphOf`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[relationship(relationship_target = Glyphs)]
pub struct GlyphOf(pub Entity);

/// Tracks which [`Glyph`] entities compose a text span.
///
/// All text span entities with store their glyphs in [`SpanGlyphs`].
#[derive(Debug, Component, Reflect)]
#[relationship_target(relationship = SpanGlyphOf)]
pub struct SpanGlyphs(Vec<Entity>);

/// Tracks the [`SpanGlyphs`] entity in this component.
///
/// All [`Glyph`] entities will store the text span entity in [`SpanGlyphOf`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[relationship(relationship_target = SpanGlyphs)]
pub struct SpanGlyphOf(pub Entity);

impl ContainsEntity for SpanGlyphOf {
    fn entity(&self) -> Entity {
        self.0
    }
}

/// Stores the number of [`Glyph`] entities in a text hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
#[component(immutable)]
pub struct GlyphCount(pub usize);

/// Represents one [`PositionedGlyph`] in a text hierarchy.
///
/// A [`Glyph`] entity is related to the text root with [`GlyphOf`] and to the
/// text span with [`SpanGlyphOf`].
///
/// To query over effect data for a glyph, use [`EffectQuery::get`], supplying the
/// text span entity contained by [`SpanGlyphOf`].
///
/// To modify the translation, scale, and rotation of a glyph, use [`GlyphVertices`].
///
/// [`EffectQuery::get`]: crate::effects::EffectQuery::get
#[derive(Debug, Clone, Component, Deref, Reflect)]
#[require(Transform, GlyphVertices)]
pub struct Glyph(pub PositionedGlyph);

/// Stores the index of the glyph in the text hierarchy.
///
/// The index range is `0`..[`GlyphCount`].
#[derive(Debug, Default, Clone, PartialEq, Deref, Component, Reflect)]
#[component(immutable)]
pub struct GlyphIndex(pub usize);

/// The product of the root [`GlobalTransform::scale`] and [`TextFont::font_size`].
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
    mut glyphs: Query<(Entity, &SpanGlyphOf), (With<GlyphScale>, With<Glyph>)>,
) {
    for (entity, gt, font) in spans.iter() {
        for (entity, _) in glyphs.iter_mut().filter(|(_, span)| span.0 == entity) {
            commands.entity(entity).insert(GlyphScale(
                gt.scale().xy() * font.font_size / DEFAULT_FONT_SIZE,
            ));
        }
    }
}

/// Vertex offsets for a [`Glyph`] entity.
///
/// The translation, scale, and rotation of each vertex is zeroed by default. The
/// color is white by default.
///
/// Modifications to vertices in [`GlyphVertices`] should be set every frame during
/// the main schedule, preferably in the [`PrettyEffectSet`] [`SystemSet`].
///
/// After the [`Main`] schedule, [`GlyphVertices`] is extracted into the render world
/// and zeroed.
///
/// [`PrettyEffectSet`]: crate::effects::PrettyEffectSet
#[derive(Debug, Default, Clone, Copy, PartialEq, Component, Reflect)]
pub struct GlyphVertices(pub(crate) [GlyphVertex; 4]);

impl GlyphVertices {
    /// Create a [`GlyphVertices`] with all elements set to `v`.
    #[inline]
    pub fn splat(v: GlyphVertex) -> Self {
        Self([v; 4])
    }

    /// Mutably access the `translation` of all the vertices.
    #[inline]
    pub fn translation(&mut self) -> GlyphVerticesTranslation<'_> {
        GlyphVerticesTranslation {
            vertices: &mut self.0,
            mask: None,
        }
    }

    /// Mutably access the `rotation` of all the vertices.
    #[inline]
    pub fn rotation(&mut self) -> GlyphVerticesRotation<'_> {
        GlyphVerticesRotation {
            vertices: &mut self.0,
            mask: None,
        }
    }

    /// Mutably access the `scale` of all the vertices.
    #[inline]
    pub fn scale(&mut self) -> GlyphVerticesScale<'_> {
        GlyphVerticesScale {
            vertices: &mut self.0,
            mask: None,
        }
    }

    /// Mutably access the `color` of all the vertices.
    #[inline]
    pub fn color(&mut self) -> GlyphVerticesColor<'_> {
        GlyphVerticesColor {
            vertices: &mut self.0,
            mask: None,
        }
    }

    /// Mutably access the vertices described by `mask`.
    pub fn mask(&mut self, mask: impl Into<VertexMask>) -> MaskedGlyphVertices<'_> {
        MaskedGlyphVertices(self, mask.into().0)
    }

    /// Access the top-left [`GlyphVertex`].
    pub fn tl(&self) -> &GlyphVertex {
        &self.0[0]
    }

    /// Access the top-right [`GlyphVertex`].
    pub fn tr(&self) -> &GlyphVertex {
        &self.0[1]
    }

    /// Access the bottom-left [`GlyphVertex`].
    pub fn bl(&self) -> &GlyphVertex {
        &self.0[2]
    }

    /// Access the bottom-right [`GlyphVertex`].
    pub fn br(&self) -> &GlyphVertex {
        &self.0[3]
    }
}

/// Mutable access to a masked selection of vertices within [`GlyphVertices`].
#[derive(Debug)]
pub struct MaskedGlyphVertices<'a>(&'a mut GlyphVertices, u8);

impl MaskedGlyphVertices<'_> {
    /// Mutably access the `translation` of all masked vertices.
    #[inline]
    pub fn translation(&mut self) -> GlyphVerticesTranslation<'_> {
        GlyphVerticesTranslation {
            vertices: &mut self.0.0,
            mask: Some(self.1),
        }
    }

    /// Mutably access the `rotation` of all masked vertices.
    #[inline]
    pub fn rotation(&mut self) -> GlyphVerticesRotation<'_> {
        GlyphVerticesRotation {
            vertices: &mut self.0.0,
            mask: Some(self.1),
        }
    }

    /// Mutably access the `scale` of all masked vertices.
    #[inline]
    pub fn scale(&mut self) -> GlyphVerticesScale<'_> {
        GlyphVerticesScale {
            vertices: &mut self.0.0,
            mask: Some(self.1),
        }
    }

    /// Mutably access the `color` of all masked vertices.
    #[inline]
    pub fn color(&mut self) -> GlyphVerticesColor<'_> {
        GlyphVerticesColor {
            vertices: &mut self.0.0,
            mask: Some(self.1),
        }
    }
}

macro_rules! for_each {
    ($ident:ident::$field:ident $field_ty:ident) => {
        impl $ident<'_> {
            /// Calls a closure on each element of the vertices.
            pub fn for_each(&mut self, f: impl FnMut(&mut $field_ty)) {
                match self.mask {
                    Some(mask) => {
                        let iter = IterMutMasked {
                            vertices: self.vertices.iter_mut(),
                            mask,
                        };
                        iter.map(|v| &mut v.$field).for_each(f);
                    }
                    None => self.vertices.iter_mut().map(|v| &mut v.$field).for_each(f),
                }
            }
        }
    };
}

macro_rules! ops {
    ($ident:ident, $($ty:ident),*) => {
        $(
            impl std::ops::AddAssign<$ty> for $ident<'_> {
                fn add_assign(&mut self, rhs: $ty) {
                    self.for_each(|value| *value += rhs);
                }
            }

            impl std::ops::SubAssign<$ty> for $ident<'_> {
                fn sub_assign(&mut self, rhs: $ty) {
                    self.for_each(|value| *value -= rhs);
                }
            }

            impl std::ops::MulAssign<$ty> for $ident<'_> {
                fn mul_assign(&mut self, rhs: $ty) {
                    self.for_each(|value| *value *= rhs);
                }
            }

            impl std::ops::DivAssign<$ty> for $ident<'_> {
                fn div_assign(&mut self, rhs: $ty) {
                    self.for_each(|value| *value /= rhs);
                }
            }

            impl std::ops::RemAssign<$ty> for $ident<'_> {
                fn rem_assign(&mut self, rhs: $ty) {
                    self.for_each(|value| *value %= rhs);
                }
            }
        )*
    };
}

macro_rules! vertex_lens {
    {
        $(#[$attr:meta])* $ident:ident::$field:ident $field_ty:ident impl $($ty:ident),*
    } => {
        $(#[$attr])*
        pub struct $ident<'a> {
            vertices: &'a mut [GlyphVertex; 4],
            mask: Option<u8>,
        }

        for_each!($ident::$field $field_ty);
        ops!($ident, $($ty),*);
    };
}

vertex_lens! {
    /// Mutable access to the `translation` component of [`GlyphVertices`].
    #[derive(Debug)]
    GlyphVerticesTranslation::translation Vec2
    impl f32, Vec2
}

vertex_lens! {
    /// Mutable access to the `rotation` component of [`GlyphVertices`].
    #[derive(Debug)]
    GlyphVerticesRotation::rotation f32
    impl f32
}

vertex_lens! {
    /// Mutable access to the `scale` component of [`GlyphVertices`].
    #[derive(Debug)]
    GlyphVerticesScale::scale Vec2
    impl f32, Vec2
}

/// Mutable access to the `color` component of [`GlyphVertices`].
#[derive(Debug)]
pub struct GlyphVerticesColor<'a> {
    vertices: &'a mut [GlyphVertex; 4],
    mask: Option<u8>,
}

for_each!(GlyphVerticesColor::color Color);

impl<'a> GlyphVerticesColor<'a> {
    /// Linearly interpolate between the vertex color and another color, by factor,
    /// storing the result in the vertex color. Factor should be between 0.0 and 1.0.
    pub fn mix(&mut self, other: Color, factor: f32) {
        self.for_each(|color| color.mix_assign(other, factor));
    }
}

/// Iterator over a masked selection of [`GlyphVertices`].
#[derive(Debug)]
pub struct IterMutMasked<'a> {
    vertices: std::slice::IterMut<'a, GlyphVertex>,
    mask: u8,
}

impl<'a> Iterator for IterMutMasked<'a> {
    type Item = &'a mut GlyphVertex;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.vertices.next()?;
            let take = self.mask & 1 != 0;
            self.mask >>= 1;
            if take {
                return Some(next);
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.mask.count_ones() as usize;
        (count, Some(count))
    }
}

/// Vertex offset of a [`Glyph`] entity.
///
/// Stored in the [`GlyphVertices`] component.
#[derive(Debug, Default, Clone, Copy, PartialEq, Reflect)]
pub struct GlyphVertex {
    /// Position of the vertex.
    pub translation: Vec2,

    /// Rotation of the vertex.
    pub rotation: f32,

    /// Scale of the vertex.
    pub scale: Vec2,

    /// Color of the vertex.
    pub color: Color,
}

impl GlyphVertex {
    /// Create a [`GlyphVertex`] from `translation`.
    #[inline]
    pub fn from_translation(translation: Vec2) -> Self {
        Self {
            translation,
            ..Default::default()
        }
    }

    /// Create a [`GlyphVertex`] from `rotation`.
    #[inline]
    pub fn from_rotation(rotation: f32) -> Self {
        Self {
            rotation,
            ..Default::default()
        }
    }

    /// Create a [`GlyphVertex`] from `scale`.
    #[inline]
    pub fn from_scale(scale: Vec2) -> Self {
        Self {
            scale,
            ..Default::default()
        }
    }

    /// Create a [`GlyphVertex`] from `color`.
    #[inline]
    pub fn from_color(color: Color) -> Self {
        Self {
            color,
            ..Default::default()
        }
    }

    /// Clear the vertex offsets.
    #[inline]
    pub fn clear(&mut self) {
        *self = Self::default();
    }

    /// Compute the [`Transform`] of the vertex.
    ///
    /// The scale is the sum of [`Vec2::ONE`] and [`Self::scale`].
    pub fn compute_transform(&self) -> Transform {
        Transform::from_translation(self.translation.extend(0.0))
            .with_rotation(Quat::from_rotation_z(self.rotation))
            .with_scale((self.scale + Vec2::ONE).extend(1.0))
    }
}

/// A bitmask for vertices in [`GlyphVertices`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Component, Reflect)]
pub struct VertexMask(u8);

impl Default for VertexMask {
    #[inline]
    fn default() -> Self {
        Self::ALL
    }
}

impl VertexMask {
    /// Contains all vertices.
    pub const ALL: Self = Self(0b1111);

    /// Contains the top-left vertex.
    pub const TL: Self = Self(0b0001);

    /// Contains the top-right vertex.
    pub const TR: Self = Self(0b0010);

    /// Contains the bottom-left vertex.
    pub const BL: Self = Self(0b0100);

    /// Contains the bottom-right vertex.
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

impl std::ops::Not for VertexMask {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl From<&VertexMask> for VertexMask {
    fn from(val: &VertexMask) -> Self {
        *val
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
            self.computed.get(glyph_of.0).map(|computed| {
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
                .get(span_entity)
                .map_err(|_| "Invalid text hierarchy: `TextSpan` has no `TextFont`")?;

            commands.spawn((
                Visibility::Inherited,
                GlyphOf(entity),
                SpanGlyphOf(span_entity),
                Glyph(glyph.clone()),
                GlyphIndex(i),
                GlyphScale(gt.scale().xy() * font.font_size / DEFAULT_FONT_SIZE),
                GlyphCount(layout.glyphs.len()),
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

fn clear_vertices(mut glyphs: Query<&mut GlyphVertices>) {
    for mut vertices in glyphs.iter_mut() {
        vertices.0.iter_mut().for_each(|v| v.clear());
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
    use std::ops::{AddAssign, MulAssign};

    use bevy::prelude::*;

    use super::*;

    fn app() -> App {
        let mut app = App::new();
        app.add_plugins((MinimalPlugins, AssetPlugin::default(), GlyphPlugin))
            .finish();
        app
    }

    fn test_str(_str: &'static str, root: impl Bundle) {
        let mut app = app();
        app.world_mut().spawn((root, PrettyText));
        // TODO: Test string equality with the spawned glyphs. This cannot be done
        // currently because the `TextLayoutInfo` will duplicate `PositionedGlyph`s
        // for wide characters, causing there to be multiple `Glyph` entities for
        // the same wide glyph.
    }

    #[test]
    fn glyph_entities() {
        roots().for_each(|(str, root)| test_str(str, root));
    }

    fn roots() -> impl Iterator<Item = (&'static str, impl Bundle)> {
        [
            "!@#$%^&*()_+-=[]{}\\|/><.,;'\"`~",
            "normal_123",
            "¯\\_(ツ)_/¯",
            "( ಠ ͜ʖರೃ)",
            "T̴̰̦̩̲̬̥̘̤̦̤̫̟̭̝̩̯̖̪̱̱̤̱̞̰̤̥̙̜̯̍̂̄̈́̀̈́̑̈́͌̉̇̂̓̓̍̋̄̽̓̾̐̇̊͊̈́̕͘͜͜͝h̶̡̧̨̡̧̙̳̰̼̻̗̰̪̻̝̹̲̙̩̭̻̤̼̺̳̰̘̺̟̺̫̯̯̪̲̳̖̰̤̼̤̞̘̥̗̜̗̬̹͎͓̻̯̫̯̗̣͎̭̥̞̦̼̮͉̯̭̟̦͈̪͇̹̩̯̰̝̯̺̳̀͑̇̓̈́̆͗̃̈̍̈́͊̈́͒̍̋̂̒͗̅̋͒͋̂̅̈́̒̅͌̃̀̔̊̆̿̐̾̏̋͊̇̐̄̂̒̊̾̔̍̂̄̈́̈́̓̌͗̑̒̍̇̆̂́̀̈́̈͗͛͌́̇̆̾̾̽̽́̊́̏̿̈́̒̽͗̔̈̎͂͂́͘̚̚̚͜͜͠͝͝͝͠ͅi̴̧̧̢̡̛̛̩̰̱̯̠̞̖̼͇̦̳͔͈̳̬̭̖̱̺̤̪̹͚̯͓̘͈̗̰̯̭̦̪̺͓̤̹",
        ]
        .into_iter()
        .map(|str| (str, Text2d::new(str)))
    }

    #[test]
    fn glyph_vertices_unmasked() {
        let mut vertices = GlyphVertices::default();
        vertices.translation().add_assign(Vec2::new(10.0, 5.0));
        for vertex in &vertices.0 {
            assert_eq!(vertex.translation, Vec2::new(10.0, 5.0));
        }

        vertices.rotation().add_assign(1.5);
        for vertex in &vertices.0 {
            assert_eq!(vertex.rotation, 1.5);
        }

        vertices.scale().add_assign(1.0);
        vertices.scale().mul_assign(Vec2::new(2.0, 3.0));
        for vertex in &vertices.0 {
            assert_eq!(vertex.scale, Vec2::new(2.0, 3.0));
        }

        vertices.color().mix(Color::srgb(1.0, 0.0, 0.0), 0.5);
        let expected_color = Color::WHITE.mix(&Color::srgb(1.0, 0.0, 0.0), 0.5);
        for vertex in &vertices.0 {
            assert_eq!(vertex.color, expected_color);
        }
    }

    #[test]
    fn glyph_vertices_masked() {
        let mut vertices = GlyphVertices::default();

        let mut masked = vertices.mask(VertexMask::TL | VertexMask::BR);
        masked.translation().add_assign(Vec2::new(5.0, 10.0));
        masked.rotation().add_assign(1.0);
        masked.scale().add_assign(Vec2::new(2.0, 3.0));

        assert_eq!(vertices.tl().translation, Vec2::new(5.0, 10.0));
        assert_eq!(vertices.tr().translation, Vec2::ZERO);
        assert_eq!(vertices.bl().translation, Vec2::ZERO);
        assert_eq!(vertices.br().translation, Vec2::new(5.0, 10.0));

        assert_eq!(vertices.tl().rotation, 1.0);
        assert_eq!(vertices.tr().rotation, 0.0);
        assert_eq!(vertices.bl().rotation, 0.0);
        assert_eq!(vertices.br().rotation, 1.0);
    }
}
