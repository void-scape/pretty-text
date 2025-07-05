use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;

pub fn derive_text_material2d_inner(input: TokenStream) -> syn::Result<TokenStream2> {
    let input: syn::DeriveInput = syn::parse(input)?;
    let ident = &input.ident;
    let fields = bevy_macro_utils::get_struct_fields(&input.data)?;

    let pretty_text_path = match input
        .attrs
        .iter()
        .find(|a| a.path().is_ident("pretty_text_path"))
    {
        Some(attr) => {
            let path: syn::Path = attr.parse_args()?;
            quote! { ::#path }
        }
        None => {
            quote! { ::bevy_pretty_text }
        }
    };

    let atlas_field = fields
        .iter()
        .find(|field| {
            field.attrs.iter().any(|attr| {
                attr.path().is_ident("material")
                    && attr
                        .parse_args::<syn::Ident>()
                        .is_ok_and(|arg| arg == "atlas")
            })
        })
        .ok_or_else(|| syn::Error::new(ident.span(), "expected 1 field with `atlas` attribute"))?;

    let atlas_ident = atlas_field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new(atlas_field.span(), "expected atlas field to be named"))?;

    let fields: Vec<_> = fields
        .iter()
        .filter(|field| {
            !field.attrs.iter().any(|attr| {
                attr.path().is_ident("material")
                    && attr
                        .parse_args::<syn::Ident>()
                        .is_ok_and(|arg| arg == "atlas" || arg == "skip")
            })
        })
        .collect();
    let field_count = fields.len();

    if field_count == 0 {
        return Ok(quote! {
            impl #pretty_text_path::material::erased::DynTextMaterial2d for #ident {
                fn dyn_text_material() -> #pretty_text_path::material::erased::BoxedDynMaterial {
                    Box::new(|args, commands, server| {
                        commands.insert(#pretty_text_path::material::PrettyTextMaterial(
                                server.add(#ident::default())));
                        Ok(())
                    })
                }
            }

            impl #pretty_text_path::material::TextMaterial2d for #ident {
                fn set_atlas(&mut self, atlas: bevy::asset::Handle<Image>) {
                    self.#atlas_ident = atlas;
                }
            }
        });
    }

    let mut arms = Vec::new();
    arms.push(quote! {
        0 => Ok(#ident::default()),
    });

    for i in 1..=field_count {
        let current_fields = fields.iter().take(i);
        let field_assignments = current_fields.enumerate().map(|(i, field)| {
            let field_name = field.ident.as_ref().unwrap();
            quote! {
                #field_name: args[#i]
                    .parse()
                    .map_err(|e| bevy::prelude::BevyError::from(format!(
                        "failed to parse argument {} for field `{}` in `{}`: {}",
                        #i, stringify!(#field_name), std::any::type_name::<#ident>(), e
                    )))?,
            }
        });

        arms.push(quote! {
            #i => Ok(#ident {
                #(#field_assignments)*
                ..Default::default()
            }),
        });
    }

    arms.push(quote! {
        _ => Err(bevy::prelude::BevyError::from(format!(
            "expected at most {} arguments for {}, got {}",
            #field_count, std::any::type_name::<#ident>(), args.len()
        ))),
    });

    Ok(quote! {
        impl #pretty_text_path::material::erased::DynTextMaterial2d for #ident {
            fn dyn_text_material() -> #pretty_text_path::material::erased::BoxedDynMaterial {
                Box::new(|args, commands, server| {
                    let component = match args.len() {
                        #(#arms)*
                    }?;
                    commands.insert(#pretty_text_path::material::PrettyTextMaterial(
                            server.add(component)));
                    Ok(())
                })
            }
        }

        impl #pretty_text_path::material::TextMaterial2d for #ident {
            fn set_atlas(&mut self, atlas: bevy::asset::Handle<Image>) {
                self.#atlas_ident = atlas;
            }
        }
    })
}
