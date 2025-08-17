use std::marker::PhantomData;
use std::sync::Arc;
use std::time::Duration;

use bevy::ecs::component::Mutable;
use bevy::ecs::world::EntityMutExcept;
use bevy::prelude::*;

use crate::glyph::{GlyphSystems, GlyphVertex, GlyphVertices};

#[derive(Debug, Clone, Copy, SystemSet, Eq, PartialEq, Hash)]
pub struct TweenSet;

pub struct TweenPlugin;

impl Plugin for TweenPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PostUpdate,
            (
                time_runner,
                restart_playhead,
                step_playhead,
                (
                    step::<f32>,
                    step::<Vec2>,
                    step::<Vec3>,
                    step::<GlyphVertices>,
                ),
            )
                .chain()
                .in_set(TweenSet),
        );

        app.configure_sets(PostUpdate, TweenSet.before(GlyphSystems::Transform));
    }
}

#[derive(Debug, Component)]
#[require(Playhead)]
pub struct TimeRunner;

fn time_runner(time: Res<Time>, mut playheads: Query<&mut Playhead, With<TimeRunner>>) {
    for mut playhead in playheads.iter_mut() {
        playhead.step(time.delta_secs());
    }
}

#[derive(Debug, Component)]
#[relationship_target(relationship = AnimationOf, linked_spawn)]
pub struct Animations(Vec<Entity>);

#[derive(Debug, Component)]
#[relationship(relationship_target = Animations)]
#[require(AnimationDuration, AnimationCurve, AnimationPlayhead)]
pub struct AnimationOf(Entity);

#[macro_export]
macro_rules! animation {
    [$($node:expr),*$(,)?] => {
       $crate::effects::tween::Animations::spawn(($(::bevy::ecs::spawn::Spawn($node)),*))
    };
}

#[derive(Debug, Default, Component)]
pub enum AnimationMode {
    #[default]
    Once,
    Repeat,
    PingPong,
}

#[derive(Debug, Component)]
pub struct KeyFrame<T>(pub T);

#[derive(Debug, Component)]
pub struct InitialValue<T>(pub T);

#[derive(Debug, Component)]
pub struct AnimationTarget(pub Entity);

#[derive(Debug, Default, Component)]
pub struct AnimationDuration(pub Duration);

impl AnimationDuration {
    pub fn from_secs(duration: f32) -> Self {
        Self(Duration::from_secs_f32(duration))
    }
}

#[derive(Debug, Component)]
pub struct AnimationCurve(pub EaseFunction);

impl Default for AnimationCurve {
    fn default() -> Self {
        Self(EaseFunction::Linear)
    }
}

#[derive(Debug, Default, Component)]
pub struct AnimationPlayhead(pub f32);

#[macro_export]
macro_rules! lens {
    ($component:ident::$field:tt) => {
        $crate::effects::tween::DynamicFieldLens::new(|component: &mut $component| {
            &mut component.$field
        })
    };
    ($component:ident) => {
        $crate::effects::tween::DynamicFieldLens::new(|component: &mut $component| component)
    };
}

pub trait Lerp:
    std::fmt::Debug + Copy + std::ops::Add<Output = Self> + Send + Sync + 'static
{
    fn lerp(self, target: Self, t: f32) -> Self;
}

macro_rules! lerp {
    ($ident:ident) => {
        impl Lerp for $ident {
            fn lerp(self, target: Self, t: f32) -> Self {
                <Self as bevy::math::VectorSpace>::lerp(self, target, t)
            }
        }
    };
}

lerp!(f32);
lerp!(Vec2);
lerp!(Vec3);

impl Lerp for GlyphVertex {
    fn lerp(self, target: Self, t: f32) -> Self {
        Self {
            translation: self.translation.lerp(target.translation, t),
            scale: self.scale.lerp(target.scale, t),
            rotation: <f32 as bevy::math::VectorSpace>::lerp(self.rotation, target.rotation, t),
        }
    }
}

impl std::ops::Add for GlyphVertex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            translation: self.translation.add(rhs.translation),
            scale: self.scale.add(rhs.scale),
            rotation: self.rotation.add(rhs.rotation),
        }
    }
}

impl Lerp for GlyphVertices {
    fn lerp(self, target: Self, t: f32) -> Self {
        Self([
            self[0].lerp(target[0], t),
            self[1].lerp(target[1], t),
            self[2].lerp(target[2], t),
            self[3].lerp(target[3], t),
        ])
    }
}

impl std::ops::Add for GlyphVertices {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self([
            self[0].add(rhs[0]),
            self[1].add(rhs[1]),
            self[2].add(rhs[2]),
            self[3].add(rhs[3]),
        ])
    }
}

#[derive(Component, Clone)]
#[require(LensMode)]
pub struct DynamicFieldLens<T>(Arc<dyn FieldLens<T>>);

impl<T> DynamicFieldLens<T> {
    pub fn new<C, F>(f: F) -> Self
    where
        C: Component<Mutability = Mutable>,
        F: Fn(&mut C) -> &mut T + Send + Sync + 'static,
        T: Lerp,
    {
        Self(Arc::new(DynamicFieldLensFn(f, PhantomData)))
    }
}

#[derive(Debug, Default, Component)]
pub enum LensMode {
    #[default]
    Overwrite,
    Accumulate,
}

type LensTarget<'w, T> = EntityMutExcept<
    'w,
    (
        DynamicFieldLens<T>,
        LensMode,
        KeyFrame<T>,
        InitialValue<T>,
        AnimationTarget,
        AnimationPlayhead,
        Step,
        AnimationOf,
        AnimationCurve,
    ),
>;

pub trait FieldLens<T>: Send + Sync + 'static
where
    T: Lerp,
{
    fn set_field(&self, entity: &mut LensTarget<T>, value: T) -> Result;
    fn accumulate(&self, entity: &mut LensTarget<T>, value: T) -> Result;
}

pub struct DynamicFieldLensFn<T, C, F>(F, PhantomData<fn(&mut C) -> &mut T>);

impl<T, C, F> FieldLens<T> for DynamicFieldLensFn<T, C, F>
where
    C: Component<Mutability = Mutable>,
    F: Fn(&mut C) -> &mut T + Send + Sync + 'static,
    T: Lerp,
{
    fn set_field(&self, entity: &mut LensTarget<T>, value: T) -> Result {
        let mut component = entity.get_mut::<C>().ok_or_else(|| {
            format!(
                "Expected component `{}` on animation target",
                std::any::type_name::<C>()
            )
        })?;
        *(self.0)(&mut component) = value;
        Ok(())
    }

    fn accumulate(&self, entity: &mut LensTarget<T>, value: T) -> Result {
        let mut component = entity.get_mut::<C>().ok_or_else(|| {
            format!(
                "Expected component `{}` on animation target",
                std::any::type_name::<C>()
            )
        })?;
        let current = (self.0)(&mut component);
        *current = <T as std::ops::Add>::add(*current, value);
        Ok(())
    }
}

#[derive(Debug, Default, Component)]
#[require(AnimationMode)]
pub struct Playhead {
    accum: f32,
    step: f32,
}

impl Playhead {
    pub fn step(&mut self, step: f32) {
        self.accum += self.step;
        self.step = step;
    }
}

#[derive(Debug, Component)]
pub struct Reverse;

fn restart_playhead(
    mut commands: Commands,
    mut playheads: Query<(
        Entity,
        &mut Playhead,
        &AnimationMode,
        &Animations,
        Has<Reverse>,
    )>,
    animations: Query<Has<Finished>, With<AnimationOf>>,
    last_steps: Query<Entity, With<Step>>,
) {
    for entity in last_steps.iter() {
        commands.entity(entity).remove::<Step>();
    }

    for (root, mut playhead, mode, entities, reverse) in playheads.iter_mut() {
        if animations
            .iter_many(entities.iter())
            .all(|finished| finished)
        {
            match mode {
                AnimationMode::Once => {
                    commands.entity(root).despawn();
                    continue;
                }
                AnimationMode::Repeat => {
                    playhead.accum = 0f32;
                    playhead.step = 0f32;
                    for entity in entities.iter() {
                        commands
                            .entity(entity)
                            .remove::<Finished>()
                            .insert(AnimationPlayhead::default());
                    }
                }
                AnimationMode::PingPong => {
                    if reverse {
                        commands.entity(root).remove::<Reverse>();
                    } else {
                        commands.entity(root).insert(Reverse);
                    }
                    playhead.accum = 0f32;
                    playhead.step = 0f32;
                    for entity in entities.iter() {
                        commands.entity(entity).remove::<Finished>();
                    }
                }
            }
        }
    }
}

fn step_playhead(
    mut commands: Commands,
    mut playheads: Query<(&Playhead, &Animations, Has<Reverse>)>,
    animations: Query<(Entity, &AnimationDuration, Has<Finished>)>,
    last_steps: Query<Entity, With<Step>>,
) {
    for entity in last_steps.iter() {
        commands.entity(entity).remove::<Step>();
    }

    for (playhead, entities, reverse) in playheads.iter_mut() {
        if reverse {
            for (entity, duration, finished) in animations.iter_many(entities.iter().rev()) {
                if finished {
                    continue;
                }

                let dur = duration.0.as_secs_f32();
                if dur == 0f32 {
                    commands.entity(entity).insert(Step(-playhead.step));
                    continue;
                }
                commands.entity(entity).insert(Step(-playhead.step / dur));
                break;
            }
        } else {
            for (entity, duration, finished) in animations.iter_many(entities.iter()) {
                if finished {
                    continue;
                }

                let dur = duration.0.as_secs_f32();
                if dur == 0f32 {
                    commands.entity(entity).insert(Step(playhead.step));
                    continue;
                }
                commands.entity(entity).insert(Step(playhead.step / dur));
                break;
            }
        }
    }
}

#[derive(Debug, Component)]
pub struct Finished;

#[derive(Debug, Component)]
pub struct Step(pub f32);

fn step<T: Lerp>(
    mut commands: Commands,
    roots: Query<Option<&AnimationTarget>, With<Animations>>,
    mut animations: Query<
        (
            Entity,
            &mut AnimationPlayhead,
            &Step,
            &AnimationOf,
            &AnimationCurve,
            &KeyFrame<T>,
            &InitialValue<T>,
        ),
        Without<Finished>,
    >,
    lenses: Query<(&DynamicFieldLens<T>, &LensMode)>,
    mut targets: Query<LensTarget<T>>,
) -> Result {
    for (animation_entity, mut playhead, step, animation_of, curve, key_frame, initial_value) in
        animations.iter_mut()
    {
        let (lens, lens_mode) = lenses.get(animation_of.0)?;
        let entity_get = match roots.get(animation_of.0)? {
            Some(target) => targets.get_mut(target.0),
            None => targets.get_mut(animation_of.0),
        };

        let mut entity = match entity_get {
            Ok(entity) => entity,
            Err(_) => {
                error!("Animation has no target, despawning.");
                commands.entity(animation_of.0).despawn();
                continue;
            }
        };

        playhead.0 += step.0;
        if playhead.0 > 1f32 || playhead.0 < 0f32 {
            playhead.0 = playhead.0.clamp(0f32, 1f32);
            commands.entity(animation_entity).insert(Finished);
        }
        let t = curve.0.sample(playhead.0).unwrap();

        let value = initial_value.0.lerp(key_frame.0, t);
        match lens_mode {
            LensMode::Overwrite => lens.0.set_field(&mut entity, value)?,
            LensMode::Accumulate => lens.0.accumulate(&mut entity, value)?,
        }
    }
    Ok(())
}
