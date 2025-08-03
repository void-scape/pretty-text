//! Module for generating parser documentation.
//!
//! [`DynamicEffectSyntax`] contains documentation and usage examples for
//! generating rustdoc at compile time and providing helpful error reports
//! at run time.
//!
//! ## Example
//!
//! ```ignore
//! /// Cycles between random, alphanumeric glyphs.
//! #[derive(Debug, Component, Reflect, DynamicEffect)]
//! #[require(PrettyText)]
//! #[parser_syntax]
//! pub struct Scramble {
//!     /// Controls the time in seconds that a glyph is retained.
//!     #[syntax(
//!         default = ScrambleSpeed::Fixed(12.0) => "12",
//!         "duration" => "fixed(duration)",
//!         "start..end" => "random(start..end)",
//!     )]
//!     pub speed: ScrambleSpeed,
//!
//!     /// Controls the time in seconds that a glyph will scramble.
//!     #[syntax(
//!         default = ScrambleLifetime::Fixed(0.5) => "0.5",
//!         "always",
//!         "duration" => "fixed(duration)",
//!         "start..end" => "random(start..end)",
//!     )]
//!     pub lifetime: ScrambleLifetime,
//! }
//! ```

use bevy::ecs::reflect::AppTypeRegistry;
use bevy::reflect::{Reflect, reflect_trait};
use convert_case::Casing;

/// Provides documentation at run-time for a [`DynamicEffect`](super::DynamicEffect).
///
/// [`DynamicEffectSyntax`] is generated automatically for built-in effects but
/// the mechanism is not exposed.
#[reflect_trait]
pub trait GetDynamicEffectSyntax {
    /// Get the [`DynamicEffectSyntax`] for this type.
    fn get(&self) -> DynamicEffectSyntax<'static>;
}

/// Convenience function for retrieving [`DynamicEffectSyntax`] from a [`Reflect`]
/// type which may or may not implement [`GetDynamicEffectSyntax`].
pub fn get_dynamic_effect_syntax<T: Reflect>(
    registry: &AppTypeRegistry,
    effect: &T,
) -> Option<DynamicEffectSyntax<'static>> {
    let registry = registry.read();
    let registration = registry.get(std::any::TypeId::of::<T>()).unwrap();
    let dynamic_syntax = registration.data::<ReflectGetDynamicEffectSyntax>()?;
    dynamic_syntax
        .get(effect.as_reflect())
        .map(GetDynamicEffectSyntax::get)
}

/// Syntax documentation for a dynamic effect.
#[derive(Debug, Clone, Copy)]
pub struct DynamicEffectSyntax<'a> {
    /// The tag of the effect.
    pub name: &'a str,
    /// All of the fields exposed to the [`DynamicEffect`](super::DynamicEffect)
    /// implementation.
    pub fields: &'a [DynamicEffectFieldSyntax<'a>],
    /// Flag for whether or not this is a [`GlyphMaterial`](crate::material::GlyphMaterial).
    pub is_material: bool,
}

impl<'a> DynamicEffectSyntax<'a> {
    /// Generate help string for this effect.
    pub fn help_fmt(&self) -> String {
        format!(
            "Usage: pretty!(\"`my text`[{}({})]\")\nSyntax for `{}`:\n{}",
            self.name,
            self.parsed_field_usage(),
            self.name,
            self.fields
                .iter()
                .map(DynamicEffectFieldSyntax::help_fmt)
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }

    /// Generate rustdoc with syntax and usage.
    pub fn doc_fmt(&self) -> String {
        format!(
            "# [Argument Syntax](pretty_text::parser)\n{}\n{}\n{}",
            self.fields
                .iter()
                .map(DynamicEffectFieldSyntax::doc_fmt)
                .collect::<Vec<_>>()
                .join("\n\n"),
            self.usage(),
            NOTES,
        )
    }

    fn usage(&self) -> String {
        let insert = if self.is_material {
            format!(
                "PrettyTextMaterial(materials.add({}::default()))",
                self.name.to_case(convert_case::Case::Pascal),
            )
        } else {
            format!(
                "{}::default()",
                self.name.to_case(convert_case::Case::Pascal),
            )
        };

        format!(
            "{0}\n// Parsed\nworld.spawn(pretty!(\"`my text`[{1}({2})]\"));\n\
                world.spawn(PrettyParser::bundle(\"`my text`[{1}({2})]\")?);\n\n\
                // Literal\nworld.spawn((\n\tText::new(\"my text\"),\n\t{3},\n));\n{4}",
            USAGE_HEADER_STR,
            self.name,
            self.parsed_field_usage(),
            insert,
            USAGE_FOOTER_STR,
        )
    }

    fn parsed_field_usage(&self) -> String {
        self.fields
            .iter()
            .map(|field| {
                let dot_stripped = field.default.strip_suffix(".0").unwrap_or(field.default);
                field.default.strip_suffix("f32").unwrap_or(dot_stripped)
            })
            .collect::<Vec<_>>()
            .join(", ")
    }
}

/// Syntax documentation for a dynamic effect field.
#[derive(Debug, Clone, Copy)]
pub struct DynamicEffectFieldSyntax<'a> {
    /// Field identifier.
    pub name: &'a str,
    /// Field rustdoc.
    pub docs: &'a str,
    /// The default value.
    pub default: &'a str,
    /// A collection of valid arguments and optional shorthand.
    pub arguments: &'a [Argument<'a>],
}

impl DynamicEffectFieldSyntax<'_> {
    /// Generate help string for this field.
    pub fn help_fmt(&self) -> String {
        format!(
            "  {}: {}\n    Valid arguments: [{}]",
            self.name,
            self.docs,
            self.arguments
                .iter()
                .map(Argument::help_fmt)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    /// Generate rustdoc for this field.
    pub fn doc_fmt(&self) -> String {
        format!(
            "`{}`: {}\n{}",
            self.name,
            self.docs,
            self.arguments
                .iter()
                .map(Argument::doc_fmt)
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

/// Argument in [`DynamicEffectFieldSyntax`].
#[derive(Debug, Clone, Copy)]
pub struct Argument<'a> {
    /// The `base` argument that `shorthand` expands to.
    pub base: &'a str,
    /// The shorthand syntax.
    pub shorthand: Option<&'a str>,
}

impl Argument<'_> {
    /// Generate syntax for this argument.
    pub fn help_fmt(&self) -> String {
        match self.shorthand {
            Some(shorthand) => format!("`\"{}\"` or `\"{}\"`", self.base, shorthand),
            None => format!("`\"{}\"`", self.base),
        }
    }

    /// Generate rustdoc for this argument.
    pub fn doc_fmt(&self) -> String {
        match self.shorthand {
            Some(shorthand) => format!("- `\"{}\"`, `\"{}\"`\n", self.base, shorthand),
            None => format!("- `\"{}\"`\n", self.base),
        }
    }
}

// rustdoc wrappers around the usage examples for syntax documentation
const USAGE_HEADER_STR: &str = r"
# Usage

```
# use pretty_text_effects::*;
# use pretty_text_macros::pretty;
# use pretty_text::dynamic_effects::PrettyTextEffectAppExt;
# use pretty_text::parser::PrettyParser;
# use pretty_text::material::PrettyTextMaterial;
# use pretty_text as bevy_pretty_text;
# use bevy::prelude::*;
#
# struct DummyMaterials;
#
# impl DummyMaterials {
#     fn add<T: bevy::prelude::Asset>(&self, _: T) -> bevy::prelude::Handle<T> {
#         bevy::prelude::Handle::default()
#     }
# }
#
# fn __test() -> bevy::prelude::Result {
#     let mut world = bevy::prelude::World::new();
#     #[allow(unused)]
#     let materials = DummyMaterials;";

const USAGE_FOOTER_STR: &str = r"
#     Ok(())
# }
# assert!(__test().is_ok());
```";

const NOTES: &str = r#"
### Notes
Positional arguments must appear in order.
Named arguments can appear after positional arguments but not before.
"#;

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for DynamicEffectSyntax<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        let name = self.name;
        let fields = self.fields;
        let is_material = self.is_material;

        tokens.append_all(quote::quote! {
            ::bevy_pretty_text::effects::dynamic::DynamicEffectSyntax {
                name: #name,
                fields: &[#(#fields,)*],
                is_material: #is_material,
            }
        });
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for DynamicEffectFieldSyntax<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::TokenStreamExt;

        let name = self.name;
        let docs = self.docs;
        let default = self.default;
        let arguments = self.arguments;

        tokens.append_all(quote::quote! {
            ::bevy_pretty_text::effects::dynamic::DynamicEffectFieldSyntax {
                name: #name,
                docs: #docs,
                default: #default,
                arguments: &[#(#arguments,)*],
            }
        });
    }
}

#[cfg(feature = "proc-macro")]
impl quote::ToTokens for Argument<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::{TokenStreamExt, quote};

        let base = self.base;
        let shorthand = match self.shorthand {
            Some(shorthand) => quote! { Some(#shorthand) },
            None => quote! { None },
        };

        tokens.append_all(quote! {
            ::bevy_pretty_text::effects::dynamic::Argument {
                base: #base,
                shorthand: #shorthand,
            }
        });
    }
}
