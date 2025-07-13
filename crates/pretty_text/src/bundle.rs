use bevy::ecs::component::HookContext;
use bevy::ecs::spawn::SpawnableList;
use bevy::ecs::world::DeferredWorld;
use bevy::prelude::*;

use crate::PrettyText;
use crate::parser::TextSpanBundle;

#[derive(Debug, Clone, Component)]
#[require(PrettyText, Text2d)]
#[component(on_add = Self::spawn)]
pub struct PrettyTextSpans(Vec<TextSpanBundle>);

impl PrettyTextSpans {
    pub fn bundle(self) -> impl Bundle {
        (
            PrettyText,
            Text2d::default(),
            Children::spawn(TextSpanSpawner::from_vec(self.0)),
        )
    }
}

impl PrettyTextSpans {
    // this is less efficient than before, but it makes the API nicer
    fn spawn(mut world: DeferredWorld, ctx: HookContext) {
        let spans = world.get::<Self>(ctx.entity).unwrap().0.clone();
        world
            .commands()
            .entity(ctx.entity)
            .remove::<Self>()
            .insert(Children::spawn(TextSpanSpawner::from_vec(spans)));
    }
}

impl PrettyTextSpans {
    #[inline]
    pub fn new(spans: Vec<TextSpanBundle>) -> Self {
        Self(spans)
    }
}

#[derive(Debug, Clone, Component)]
#[require(PrettyText, Text2d)]
#[component(on_add = Self::spawn)]
pub struct StaticPrettyTextSpans(&'static [TextSpanBundle]);

impl StaticPrettyTextSpans {
    pub fn bundle(self) -> impl Bundle {
        (
            PrettyText,
            Text2d::default(),
            Children::spawn(TextSpanSpawner::from_slice(self.0)),
        )
    }
}

impl StaticPrettyTextSpans {
    // this is less efficient than before, but it makes the API nicer
    fn spawn(mut world: DeferredWorld, ctx: HookContext) {
        let spans = world.get::<Self>(ctx.entity).unwrap().0;
        world
            .commands()
            .entity(ctx.entity)
            .remove::<Self>()
            .insert(Children::spawn(TextSpanSpawner::from_slice(spans)));
    }
}

impl StaticPrettyTextSpans {
    #[inline]
    pub fn new(spans: &'static [TextSpanBundle]) -> Self {
        Self(spans)
    }
}

#[derive(Debug)]
pub enum TextSpanSpawner {
    Vec(std::vec::IntoIter<TextSpanBundle>),
    Slice(core::slice::Iter<'static, TextSpanBundle>),
}

impl TextSpanSpawner {
    pub fn from_vec(spans: Vec<TextSpanBundle>) -> Self {
        Self::Vec(spans.into_iter())
    }

    pub fn from_slice(spans: &'static [TextSpanBundle]) -> Self {
        Self::Slice(spans.iter())
    }
}

impl SpawnableList<ChildOf> for TextSpanSpawner {
    fn spawn(self, world: &mut World, entity: Entity) {
        let mut commands = world.entity_mut(entity);
        match self {
            Self::Vec(vec) => {
                for span in vec {
                    span.with_parent(&mut commands);
                }
            }
            Self::Slice(arr) => {
                for span in arr {
                    span.clone().with_parent(&mut commands);
                }
            }
        }
    }

    fn size_hint(&self) -> usize {
        match &self {
            Self::Vec(vec) => vec.len(),
            Self::Slice(slice) => slice.len(),
        }
    }
}
