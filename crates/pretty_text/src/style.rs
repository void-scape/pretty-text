use std::borrow::Cow;

use bevy::color::palettes::css::{BLUE, GREEN, RED};
use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::PrettyTextSystems;

pub struct StylePlugin;

impl Plugin for StylePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<PrettyStyleRegistry>()
            .add_systems(
                PostUpdate,
                apply_span_style.in_set(PrettyTextSystems::Style),
            )
            .register_pretty_style("red", |_| Color::from(RED))
            .register_pretty_style("green", |_| Color::from(GREEN))
            .register_pretty_style("blue", |_| Color::from(BLUE));

        app.register_type::<PrettyStyleRegistry>()
            .register_type::<PrettyStyle>()
            .register_type::<SpanStyle>();
    }
}

pub trait StyleAppExt {
    fn register_pretty_style<S>(
        &mut self,
        tag: impl Into<String>,
        style: impl Fn(&AssetServer) -> S + Send + Sync + 'static,
    ) -> &mut Self
    where
        S: Into<PrettyStyle>;
}

impl StyleAppExt for App {
    fn register_pretty_style<S>(
        &mut self,
        tag: impl Into<String>,
        style: impl Fn(&AssetServer) -> S + Send + Sync + 'static,
    ) -> &mut Self
    where
        S: Into<PrettyStyle>,
    {
        let tag: &'static str = String::leak(tag.into());
        self.add_systems(
            Startup,
            move |server: Res<AssetServer>, mut registry: ResMut<PrettyStyleRegistry>| {
                registry.0.insert(tag, style(&server).into());
            },
        )
    }
}

#[derive(Debug, Default, Clone, Reflect)]
pub struct PrettyStyle {
    font: Option<TextFont>,
    color: Option<TextColor>,
}

impl PrettyStyle {
    pub fn new(font: TextFont, color: impl Into<Color>) -> Self {
        Self {
            font: Some(font),
            color: Some(TextColor(color.into())),
        }
    }

    pub fn from_color(color: impl Into<Color>) -> Self {
        Self {
            color: Some(TextColor(color.into())),
            ..Default::default()
        }
    }

    pub fn from_font(font: TextFont) -> Self {
        Self {
            font: Some(font),
            ..Default::default()
        }
    }
}

impl From<TextFont> for PrettyStyle {
    fn from(value: TextFont) -> Self {
        Self::from_font(value)
    }
}

impl From<Color> for PrettyStyle {
    fn from(value: Color) -> Self {
        Self::from_color(value)
    }
}

#[derive(Debug, Default, Resource, Reflect)]
pub struct PrettyStyleRegistry(HashMap<&'static str, PrettyStyle>);

impl PrettyStyleRegistry {
    pub fn register(&mut self, tag: impl Into<String>, style: impl Into<PrettyStyle>) {
        self.0.insert(String::leak(tag.into()), style.into());
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Component, Reflect)]
pub enum SpanStyle {
    #[default]
    Inherit,
    Tag(Cow<'static, str>),
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for SpanStyle {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Inherit => quote::quote! { ::bevy_pretty_text::style::SpanStyle::Inherit },
            Self::Tag(tag) => {
                quote::quote! { ::bevy_pretty_text::style::SpanStyle::Tag(std::borrow::Cow::Borrowed(#tag)) }
            }
        });
    }
}

fn apply_span_style(
    mut commands: Commands,
    roots: Query<(&TextFont, &TextColor)>,
    spans: Query<(Entity, &SpanStyle, &ChildOf), Added<SpanStyle>>,
    registry: Res<PrettyStyleRegistry>,
) -> Result {
    for (entity, style, child_of) in spans.iter() {
        let (font, color) = roots.get(child_of.parent())?;
        match style {
            SpanStyle::Inherit => {
                commands.entity(entity).insert((font.clone(), *color));
            }
            SpanStyle::Tag(tag) => {
                let style = registry.0.get(tag.as_ref()).ok_or_else(|| {
                    format!("failed to apply text style: `{tag}` is not registered")
                })?;

                commands.entity(entity).insert((
                    style.font.clone().unwrap_or_else(|| font.clone()),
                    style.color.unwrap_or(*color),
                ));
            }
        }
    }

    Ok(())
}
