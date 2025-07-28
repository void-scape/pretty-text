//! Provides the dynamic representation of values supplied to the parser for
//! constructing styles and dynamic effects.
//!
//! See [`parser`](crate::parser#modifiers) for more information.

use std::borrow::Cow;

use bevy::prelude::*;

use crate::dynamic_effects::{DynEffectRegistry, TrackedSpan};
use crate::style::{PrettyStyle, PrettyStyleRegistry};

/// A comma separated collection of [effects](crate::dynamic_effects) and [styles](crate::style)
/// directly following a [`Span`](crate::parser::Span), contained within square
/// brackets: `"[mod1, ...]"`.
///
/// See [`Modifier`].
#[derive(Debug, Default, Clone, Component, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Modifiers(pub Vec<Modifier>);

/// A dynamic representation of a [modifier](crate::parser#modifiers).
///
/// Constructed by [`parser`](crate::parser) to dynamically insert text effects.
/// To apply a [`Modifier`] to text, insert the [`Modifiers`] component.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Modifier {
    /// Tag associated to a [registered dynamic effect].
    ///
    /// [registered dynamic effect]: crate::dynamic_effects::PrettyTextEffectAppExt
    pub tag: Tag,

    /// Field arguments for a dynamic effect.
    pub args: Vec<Arg>,
}

/// Tag associated to a [registered dynamic effect].
///
/// [registered dynamic effect]: crate::dynamic_effects::PrettyTextEffectAppExt
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub struct Tag(Cow<'static, str>);

impl AsRef<str> for Tag {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<&'static str> for Tag {
    fn from(value: &'static str) -> Self {
        Self(Cow::Borrowed(value))
    }
}

impl From<String> for Tag {
    fn from(value: String) -> Self {
        Self(Cow::Owned(value))
    }
}

impl From<Cow<'static, str>> for Tag {
    fn from(value: Cow<'static, str>) -> Self {
        Self(value)
    }
}

/// Field arguments for a dynamic effect.
///
/// See [`ArgParser`](crate::parser::ArgParser) for a description of the argument syntax.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serialize", reflect(Serialize, Deserialize))]
pub enum Arg {
    /// Positional argument: `value`.
    Positioned(Cow<'static, str>),
    /// Named argument: `field=value`.
    Named {
        /// Name of a field.
        field: Cow<'static, str>,
        /// Value to assign to `field`.
        value: Cow<'static, str>,
    },
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Positioned(arg) => f.write_str(arg.as_ref()),
            Self::Named { field, value } => {
                write!(f, "{}={}", field.as_ref(), value.as_ref())
            }
        }
    }
}

impl From<&'static str> for Arg {
    fn from(value: &'static str) -> Self {
        Self::Positioned(value.into())
    }
}

impl From<String> for Arg {
    fn from(value: String) -> Self {
        Self::Positioned(value.into())
    }
}

pub(crate) fn apply_modifiers(
    trigger: Trigger<OnAdd, Modifiers>,
    mut commands: Commands,
    registry: Res<AppTypeRegistry>,
    server: Res<AssetServer>,
    effect_registry: Res<DynEffectRegistry>,
    style_registry: Res<PrettyStyleRegistry>,
    mods: Query<(Entity, &Modifiers, Option<&ChildOf>, Option<&TrackedSpan>), Added<Modifiers>>,
    text_components: Query<(&TextFont, &TextColor)>,
) -> Result {
    let (entity, mods, child_of, tracked) = mods.get(trigger.target())?;

    // inherit font and color first
    if let Some(child_of) = child_of {
        if let Ok((font, color)) = text_components.get(child_of.0) {
            commands.entity(entity).insert((font.clone(), *color));
        }
    }

    for modifier in mods.0.iter() {
        if let Some(handler) = effect_registry.get(modifier.tag.as_ref()) {
            if let Err(mut err) = handler.insert_from_args(
                &registry,
                &server,
                &mut commands.entity(trigger.target()),
                &modifier.args,
            ) {
                if let Some(tracked) = tracked {
                    err.tracked(tracked.location());
                }
                return Err(err.into());
            }
        } else if let Some(style_entity) = style_registry.get(modifier.tag.as_ref()) {
            if !modifier.args.is_empty() {
                error!(
                    "expected no arguments for style `{}`",
                    modifier.tag.as_ref()
                );
            }

            commands.entity(*style_entity).clone_with(entity, |config| {
                config.deny::<PrettyStyle>();
            });
        } else {
            error!("modifier `{}` is not registered", modifier.tag.as_ref());
        }
    }

    Ok(())
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for Modifier {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        let tag = self.tag.as_ref();
        let args = &self.args;
        tokens.append_all(quote::quote! {
            bevy_pretty_text::modifier::Modifier {
                tag: #tag.into(),
                args: vec![#(#args),*]
            }
        });
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for Arg {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        match self {
            Self::Positioned(tag) => {
                let tag = tag.as_ref();
                tokens.append_all(quote::quote! {
                    bevy_pretty_text::modifier::Arg::Positioned(#tag.into())
                });
            }
            Self::Named { field, value } => {
                let field = field.as_ref();
                let value = value.as_ref();
                tokens.append_all(quote::quote! {
                    bevy_pretty_text::modifier::Arg::Named {
                        field: #field.into(),
                        value: #value.into(),
                    }
                });
            }
        }
    }
}
