use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::material::erased::{DynMaterialRegistry, ErasedPrettyTextMaterial};
use crate::parser::PrettyTextEffect;

pub trait DynamicEffect: Send + Sync + 'static {
    fn insert_from_args(&self, args: &[String], entity: &mut EntityCommands) -> Result<()>;
}

pub trait PrettyTextEffectAppExt {
    fn register_pretty_effect<T: Default + DynamicEffect>(
        &mut self,
        tag: &'static str,
    ) -> &mut Self;
}

impl PrettyTextEffectAppExt for App {
    fn register_pretty_effect<T: Default + DynamicEffect>(
        &mut self,
        tag: &'static str,
    ) -> &mut Self {
        self.add_systems(PreStartup, register_dyn_effect::<T>(tag))
    }
}

fn register_dyn_effect<T: Default + DynamicEffect>(
    tag: &'static str,
) -> impl Fn(ResMut<DynEffectRegistry>) {
    move |mut registry| {
        registry.insert(tag, Box::new(T::default()));
    }
}

#[derive(Default, Deref, DerefMut, Resource)]
pub(crate) struct DynEffectRegistry(pub HashMap<&'static str, Box<dyn DynamicEffect>>);

pub(crate) fn text_effect(
    trigger: Trigger<OnAdd, PrettyTextEffect>,
    mut commands: Commands,
    material_registry: Res<DynMaterialRegistry>,
    effects_registry: Res<DynEffectRegistry>,
    new_effects: Query<&PrettyTextEffect>,
) -> Result {
    let effect = new_effects.get(trigger.target())?;
    if material_registry.0.get(effect.tag.as_str()).is_some() {
        commands
            .entity(trigger.target())
            .insert(ErasedPrettyTextMaterial {
                tag: effect.tag.clone(),
                args: effect.args.clone(),
            });
    } else if let Some(handler) = effects_registry.get(effect.tag.as_str()) {
        handler.insert_from_args(&effect.args, &mut commands.entity(trigger.target()))?;
    } else {
        return Err(format!("effect `{}` is not registered", effect.tag).into());
    }

    commands
        .entity(trigger.target())
        .remove::<PrettyTextEffect>();

    Ok(())
}
