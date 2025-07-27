use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

use crate::ATTR_IDENT;

pub fn derive_dynamic_effect_inner(input: TokenStream) -> syn::Result<TokenStream2> {
    let input: syn::DeriveInput = syn::parse(input)?;
    let ident = &input.ident;
    let fields = bevy_macro_utils::get_struct_fields(&input.data)?;
    let pretty_text_path = quote! { bevy_pretty_text };

    let is_material = input.attrs.iter().any(|attr| {
        attr.path().is_ident(ATTR_IDENT)
            && attr
                .parse_args::<syn::Ident>()
                .is_ok_and(|arg| arg == "material")
    });

    let skip = fields
        .iter()
        .filter(|field| {
            field.attrs.iter().any(|attr| {
                attr.path().is_ident(ATTR_IDENT)
                    && attr
                        .parse_args::<syn::Ident>()
                        .is_ok_and(|arg| arg == "skip" || arg == "atlas")
            })
        })
        .enumerate()
        .map(|(i, _)| quote! { last_positioned == #i });

    let fields: Vec<_> = fields
        .iter()
        .filter(|field| {
            !field.attrs.iter().any(|attr| {
                attr.path().is_ident(ATTR_IDENT)
                    && attr
                        .parse_args::<syn::Ident>()
                        .is_ok_and(|arg| arg == "skip" || arg == "atlas")
            })
        })
        .collect();

    let positioned_arms = fields.iter().enumerate().map(|(i, field)| {
        let ident = field.ident.as_ref().unwrap();

        quote! {
            #i => {
                component.#ident =
                    match <_ as bevy_pretty_text::parser::ArgParser>::parse_arg.parse(value.as_ref()) {
                        Ok(value) => value,
                        Err(error) => {
                            return Err(
                                bevy_pretty_text::dynamic_effects::DynamicEffectError::from_effect(
                                    &registry,
                                    &component,
                                    bevy_pretty_text::dynamic_effects::ErrorKind::Parser {
                                        effect: std::any::type_name::<Self>(),
                                        field: stringify!(#ident),
                                        arg: arg.clone(),
                                        error: error.to_string(),
                                    },
                                ),
                            );
                        }
                    };
            }
        }
    });

    let named_arms = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();

        quote! {
            stringify!(#ident) => {
                component.#ident =
                    match <_ as bevy_pretty_text::parser::ArgParser>::parse_arg.parse(value.as_ref()) {
                        Ok(value) => value,
                        Err(error) => {
                            return Err(
                                bevy_pretty_text::dynamic_effects::DynamicEffectError::from_effect(
                                    &registry,
                                    &component,
                                    bevy_pretty_text::dynamic_effects::ErrorKind::Parser {
                                        effect: std::any::type_name::<Self>(),
                                        field: stringify!(#ident),
                                        arg: arg.clone(),
                                        error: error.to_string(),
                                    },
                                ),
                            );
                        }
                    };
            }
        }
    });

    let field_name_index = fields.iter().enumerate().map(|(i, field)| {
        let ident = field.ident.as_ref().unwrap();
        quote! {
            stringify!(#ident) => last_positioned >= #i,
        }
    });

    let insert = if is_material {
        quote! {
            entity.insert(
                #pretty_text_path::material::PrettyTextMaterial(server.add(component)),
            );
        }
    } else {
        quote! { entity.insert(component); }
    };

    Ok(quote! {
        #[automatically_derived]
        impl bevy_pretty_text::dynamic_effects::DynamicEffect for #ident {
            fn insert_from_args(
                &self,
                registry: &bevy::prelude::AppTypeRegistry,
                server: &bevy::prelude::AssetServer,
                entity: &mut bevy::prelude::EntityCommands,
                args: &[bevy_pretty_text::modifier::Arg],
            ) -> bevy_pretty_text::dynamic_effects::DynamicEffectResult {
                use ::winnow::Parser;
                let mut component = Self::default();
                let mut supplied_named = false;
                let mut supplied_position = false;
                let mut last_positioned = 0;
                for arg in args.iter() {
                    match arg {
                        bevy_pretty_text::modifier::Arg::Positioned(value) => {
                            if supplied_named {
                                return Err(
                                    bevy_pretty_text::dynamic_effects::DynamicEffectError::from_effect(
                                        &registry,
                                        &component,
                                        bevy_pretty_text::dynamic_effects::ErrorKind::InvalidPositionalArg {
                                            effect:std::any::type_name:: <Self>(),
                                        },
                                    )
                                );
                            }
                            while #(#skip||)* false {
                                last_positioned += 1;
                            }

                            supplied_position = true;
                            match last_positioned {
                                #(#positioned_arms)*
                                _ => {
                                    return Err(
                                        bevy_pretty_text::dynamic_effects::DynamicEffectError::from_effect(
                                            &registry,
                                            &component,
                                            bevy_pretty_text::dynamic_effects::ErrorKind::TooManyArgs {
                                                effect: std::any::type_name::<Self>(),
                                            },
                                        ),
                                    );
                                }
                            };
                            last_positioned += 1;
                        }
                        bevy_pretty_text::modifier::Arg::Named {
                            field: field_name,
                            value,
                        } => {
                            if supplied_position
                                && {
                                    match field_name.as_ref() {
                                        #(#field_name_index)*
                                        _ => false
                                    }
                                }
                            {
                                return Err(
                                    bevy_pretty_text::dynamic_effects::DynamicEffectError::from_effect(
                                        &registry,
                                        &component,
                                        bevy_pretty_text::dynamic_effects::ErrorKind::NamedArgOverride {
                                            name: field_name.as_ref().to_string(),
                                            effect: std::any::type_name::<Self>(),
                                        },
                                    ),
                                );
                            }

                            supplied_named = true;
                            match field_name.as_ref() {
                                #(#named_arms)*
                                _ => {
                                    return Err(
                                        bevy_pretty_text::dynamic_effects::DynamicEffectError::from_effect(
                                            &registry,
                                            &component,
                                            bevy_pretty_text::dynamic_effects::ErrorKind::InvalidNamedArg {
                                                name: field_name.as_ref().to_string(),
                                                effect: std::any::type_name::<Self>(),
                                            },
                                        ),
                                    );
                                }
                            };
                        }
                    }
                }
                #insert;
                Ok(())
            }
        }
    })
}
