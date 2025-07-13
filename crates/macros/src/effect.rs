use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

const ATTR_IDENT: &str = "text_effect";

pub fn derive_text_effect_inner(input: TokenStream) -> syn::Result<TokenStream2> {
    let input: syn::DeriveInput = syn::parse(input)?;
    let ident = &input.ident;
    let fields = bevy_macro_utils::get_struct_fields(&input.data)?;
    let pretty_text_path = quote! { ::bevy_pretty_text };

    let original_count = fields.len();
    let fields: Vec<_> = fields
        .iter()
        .filter(|field| {
            !field.attrs.iter().any(|attr| {
                attr.path().is_ident(ATTR_IDENT)
                    && attr
                        .parse_args::<syn::Ident>()
                        .is_ok_and(|arg| arg == "skip")
            })
        })
        .collect();
    let field_count = fields.len();

    if field_count == 0 {
        return Ok(quote! {
            impl #pretty_text_path::dynamic_effects::DynamicEffect for #ident {
                fn insert_from_args(
                    &self,
                    args: &[std::borrow::Cow<'static, str>],
                    entity: &mut bevy::prelude::EntityCommands,
                ) -> bevy::prelude::Result<()> {
                    if !args.is_empty() {
                        return Err(
                            bevy::prelude::BevyError::from(format!(
                                "expected no arguments for {}, got {}",
                                std::any::type_name::<#ident>(),
                                args.len()
                            ))
                        );
                    }
                    entity.insert(#ident::default());
                    Ok(())
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

        let base = (original_count != field_count || i != field_count)
            .then_some(quote! { ..Default::default() })
            .unwrap_or_default();

        arms.push(quote! {
            #i => Ok(#ident {
                #(#field_assignments)*
                #base
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
        impl #pretty_text_path::dynamic_effects::DynamicEffect for #ident {
            fn insert_from_args(
                &self,
                args: &[std::borrow::Cow<'static, str>],
                entity: &mut bevy::prelude::EntityCommands,
            ) -> bevy::prelude::Result<()> {
                let component = match args.len() {
                    #(#arms)*
                }?;
                entity.insert(component);
                Ok(())
            }
        }
    })
}
