use std::borrow::Cow;

use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::material::erased::{DynMaterialRegistry, ErasedPrettyTextMaterial};

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
        self.add_systems(
            PreStartup,
            move |mut registry: ResMut<DynEffectRegistry>| {
                registry.insert(tag, Box::new(T::default()));
            },
        )
    }
}

pub trait DynamicEffect: Send + Sync + 'static {
    fn insert_from_args(
        &self,
        args: &[Cow<'static, str>],
        entity: &mut EntityCommands,
    ) -> Result<()>;
}

#[derive(Default, Deref, DerefMut, Resource)]
pub struct DynEffectRegistry(pub HashMap<&'static str, Box<dyn DynamicEffect>>);

#[derive(Component)]
pub struct PrettyTextEffectCollection(pub Cow<'static, [PrettyTextEffect]>);

#[derive(Debug, Clone)]
pub struct PrettyTextEffect {
    pub tag: Cow<'static, str>,
    pub args: Cow<'static, [Cow<'static, str>]>,
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for PrettyTextEffect {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;
        let tag = &self.tag;
        let args = &self.args;
        tokens.append_all(quote::quote! {
            ::bevy_pretty_text::dynamic_effects::PrettyTextEffect {
                tag: std::borrow::Cow::Borrowed(#tag),
                args: std::borrow::Cow::Borrowed(&[#(std::borrow::Cow::Borrowed(#args),)*])
            }
        });
    }
}

pub(crate) fn text_effect(
    trigger: Trigger<OnAdd, PrettyTextEffectCollection>,
    mut commands: Commands,
    material_registry: Res<DynMaterialRegistry>,
    effects_registry: Res<DynEffectRegistry>,
    new_effects: Query<&PrettyTextEffectCollection>,
) -> Result {
    let effects = new_effects.get(trigger.target())?;

    let mut material = None;
    for effect in effects.0.iter() {
        if material_registry.0.get(effect.tag.as_ref()).is_some() {
            if let Some(other) = material {
                return Err(format!(
                    "registered multiple materials on single span: `{}` and `{}`",
                    other, effect.tag
                )
                .into());
            }

            material = Some(&effect.tag);
            commands
                .entity(trigger.target())
                .insert(ErasedPrettyTextMaterial {
                    tag: effect.tag.clone(),
                    args: effect.args.clone(),
                });
        } else if let Some(handler) = effects_registry.get(effect.tag.as_ref()) {
            handler.insert_from_args(&effect.args, &mut commands.entity(trigger.target()))?;
        } else {
            return Err(format!("effect `{}` is not registered", effect.tag).into());
        }
    }

    commands
        .entity(trigger.target())
        .remove::<PrettyTextEffectCollection>();

    Ok(())
}
