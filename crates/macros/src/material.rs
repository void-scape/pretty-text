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
        .find(|field| field.attrs.iter().any(|attr| attr.path().is_ident("atlas")))
        .ok_or_else(|| syn::Error::new(ident.span(), "expected 1 field with `atlas` attribute"))?;

    let atlas_ident = atlas_field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new(atlas_field.span(), "expected atlas field to be named"))?;

    let other_fields: Vec<_> = fields
        .iter()
        .filter(|field| !field.attrs.iter().any(|attr| attr.path().is_ident("atlas")))
        .collect();
    let field_count = other_fields.len();

    let field_names = other_fields.iter().enumerate().map(|(i, field)| {
        // if atlas is named, these must also be named
        let field_name = field.ident.as_ref().unwrap();
        quote! {
            #field_name: if #i < args.len() {
                args[#i]
                    .parse()
                    .map_err(|e|
                        format!(
                            "failed to parse argument {} for field `{}` in `{}`: {}",
                            #i,
                            stringify!(#field_name),
                            std::any::type_name::<#ident>(),
                            e,
                        ))?
            } else {
                Default::default()
            }
        }
    });

    let field_assignments = quote! { #(#field_names,)* };
    let short_circuit = if field_count == 0 {
        quote! {
            if !args.is_empty() {
                return Err(
                    format!(
                        "expected no arguments for `{}`, got {}",
                        std::any::type_name::<#ident>(),
                        args.len()
                    ).into()
                );
            }
        }
    } else {
        quote! {
            if args.len() > #field_count {
                return Err(
                    format!(
                        "expected at most {} arguments for `{}`, got {}",
                        #field_count,
                        std::any::type_name::<#ident>(),
                        args.len()
                    ).into()
                );
            }
        }
    };

    Ok(quote! {
        impl #pretty_text_path::material::erased::DynTextMaterial2d for #ident {
            fn dyn_text_material() -> #pretty_text_path::material::erased::BoxedDynMaterial {
                Box::new(|args, commands, server| {
                    #short_circuit

                    commands.insert(
                        #pretty_text_path::material::PrettyTextMaterial(
                            server.add(
                                #ident {
                                    #atlas_ident: bevy::asset::Handle::default(),
                                    #field_assignments
                                },
                            )
                        )
                    );

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
