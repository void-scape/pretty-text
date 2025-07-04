use bevy::color::palettes::css::{BLUE, GREEN, RED};
use bevy::platform::collections::HashMap;
use bevy::prelude::*;

use crate::PrettyTextSystems;

#[derive(Default, Clone)]
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
        let tag = tag.into();
        self.add_systems(
            Startup,
            move |server: Res<AssetServer>, mut registry: ResMut<PrettyStyleRegistry>| {
                registry.0.insert(tag.clone(), style(&server).into());
            },
        )
    }
}

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
    }
}

#[derive(Default, Resource)]
pub struct PrettyStyleRegistry(HashMap<String, PrettyStyle>);

impl PrettyStyleRegistry {
    pub fn register(&mut self, tag: impl Into<String>, style: impl Into<PrettyStyle>) {
        self.0.insert(tag.into(), style.into());
    }
}

#[derive(Default, Component)]
pub enum SpanStyle {
    #[default]
    Default,
    Inherit,
    Tag(String),
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for SpanStyle {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        tokens.append_all(match self {
            Self::Default => quote::quote! { ::bevy_pretty_text::style::SpanStyle::Default },
            Self::Inherit => quote::quote! { ::bevy_pretty_text::style::SpanStyle::Inherit },
            Self::Tag(tag) => {
                quote::quote! { ::bevy_pretty_text::style::SpanStyle::Tag(#tag.into()) }
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
            SpanStyle::Default => {}
            SpanStyle::Inherit => {
                commands
                    .entity(entity)
                    .insert((font.clone(), color.clone()));
            }
            SpanStyle::Tag(tag) => {
                let style = registry.0.get(tag).ok_or_else(|| {
                    format!("failed to apply text style: `{}` is not registered", tag)
                })?;

                commands.entity(entity).insert((
                    style.font.clone().unwrap_or_else(|| font.clone()),
                    style.color.clone().unwrap_or(*color),
                ));
            }
        }
    }

    Ok(())
}
