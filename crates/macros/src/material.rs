use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;

use crate::ATTR_IDENT;

pub fn derive_glyph_material_inner(input: TokenStream) -> syn::Result<TokenStream2> {
    let input: syn::DeriveInput = syn::parse(input)?;
    let ident = &input.ident;
    let fields = bevy_macro_utils::get_struct_fields(&input.data)?;
    let pretty_text_path = quote! { ::bevy_pretty_text };

    let atlas_field = fields
        .iter()
        .find(|field| {
            field.attrs.iter().any(|attr| {
                attr.path().is_ident(ATTR_IDENT)
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

    Ok(quote! {
        #[automatically_derived]
        impl #pretty_text_path::effects::material::GlyphMaterial for #ident {
            fn set_atlas(&mut self, atlas: ::bevy::asset::Handle<Image>) {
                self.#atlas_ident = atlas;
            }
        }
    })
}
