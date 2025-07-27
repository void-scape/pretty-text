use convert_case::Casing;
use pretty_text::dynamic_effects::syntax::{
    Argument, DynamicEffectFieldSyntax, DynamicEffectSyntax,
};
use quote::{ToTokens, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Attribute, Expr, Ident, LitStr, Token};

use crate::ATTR_IDENT;

enum SyntaxItem {
    Default(Expr, Option<LitStr>),
    Argument(LitStr, Option<LitStr>),
}

impl Parse for SyntaxItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && input.peek2(Token![=]) {
            let ident: Ident = input.parse()?;
            if ident != "default" {
                return Err(syn::Error::new(ident.span(), "expected `default`"));
            }
            input.parse::<Token![=]>()?;

            let expr: Expr = input.parse()?;
            let alias: Option<LitStr> = if input.peek(Token![=>]) {
                input.parse::<Token![=>]>()?;
                Some(input.parse()?)
            } else {
                None
            };

            Ok(SyntaxItem::Default(expr, alias))
        } else if lookahead.peek(LitStr) {
            let first: LitStr = input.parse()?;
            if input.peek(Token![=>]) {
                input.parse::<Token![=>]>()?;
                Ok(SyntaxItem::Argument(input.parse()?, Some(first)))
            } else {
                Ok(SyntaxItem::Argument(first, None))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Default)]
struct SyntaxAttributeContent {
    default: String,
    default_alias: Option<String>,
    arguments: Vec<(String, Option<String>)>,
}

impl Parse for SyntaxAttributeContent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let items = Punctuated::<SyntaxItem, Token![,]>::parse_terminated(input)?;
        let mut content = SyntaxAttributeContent::default();
        for item in items {
            match item {
                SyntaxItem::Default(expr, alias) => {
                    if !content.default.is_empty() {
                        return Err(syn::Error::new(
                            expr.span(),
                            "default value specified more than once",
                        ));
                    }
                    content.default = expr.to_token_stream().to_string();
                    content.default_alias = alias.map(|alias| alias.value());
                }
                SyntaxItem::Argument(lit_str, shorthand) => {
                    content
                        .arguments
                        .push((lit_str.value(), shorthand.as_ref().map(LitStr::value)));
                }
            }
        }

        Ok(content)
    }
}

pub fn dynamic_effect_docs_inner(
    attr: proc_macro2::TokenStream,
    mut input: syn::ItemStruct,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut default_fields = Vec::new();

    let is_material = input.attrs.iter().any(|attr| {
        attr.path().is_ident(ATTR_IDENT)
            && attr
                .parse_args::<syn::Ident>()
                .is_ok_and(|arg| arg == "material")
    });

    let mut field_syntaxes = Vec::new();
    if let syn::Fields::Named(fields) = &mut input.fields {
        for field in &mut fields.named {
            if field.attrs.iter().any(|attr| {
                attr.path().is_ident(ATTR_IDENT)
                    && attr
                        .parse_args::<syn::Ident>()
                        .is_ok_and(|arg| arg == "atlas" || arg == "skip")
            }) {
                let field_ident = field.ident.as_ref().unwrap();
                default_fields.push(quote! { #field_ident: Default::default() });
                continue;
            }

            let field_name = field.ident.as_ref().unwrap().to_string();
            let docs = field
                .attrs
                .iter()
                .filter_map(|attr| {
                    if attr.path().is_ident("doc") {
                        if let syn::Meta::NameValue(meta) = &attr.meta {
                            if let syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Str(lit_str),
                                ..
                            }) = &meta.value
                            {
                                return Some(lit_str.value().trim().to_string());
                            }
                        }
                    }
                    None
                })
                .collect::<Vec<_>>()
                .join("\n");

            let mut syntax_content = SyntaxAttributeContent::default();
            if let Some(syntax_attr) = field.attrs.iter().find(|a| a.path().is_ident("syntax")) {
                syntax_content = syntax_attr.parse_args::<SyntaxAttributeContent>()?;
            }
            field.attrs.retain(|attr| !attr.path().is_ident("syntax"));

            if syntax_content.default.is_empty() {
                return Err(syn::Error::new(field.span(), "must provide a default case"));
            }

            let field_ident = field.ident.as_ref().unwrap();
            let default: proc_macro2::TokenStream = syntax_content.default.parse()?;
            default_fields.push(quote! { #field_ident: #default });

            if syntax_content.arguments.is_empty() {
                return Err(syn::Error::new(
                    field.span(),
                    "needs atleast 1 argument case",
                ));
            }

            field_syntaxes.push(DynamicEffectFieldSyntax {
                name: Box::leak(field_name.into_boxed_str()),
                docs: Box::leak(
                    docs.split_terminator("\n")
                        .next()
                        .map(|str| str.to_string().into_boxed_str())
                        .unwrap_or_else(|| docs.into_boxed_str()),
                ),
                default: Box::leak(
                    syntax_content
                        .default_alias
                        .unwrap_or(syntax_content.default)
                        .into_boxed_str(),
                ),
                arguments: Box::leak(
                    syntax_content
                        .arguments
                        .into_iter()
                        .map(|(base, shorthand)| Argument {
                            base: Box::leak(base.into_boxed_str()),
                            shorthand: shorthand
                                .map(|shorthand| &*Box::leak(shorthand.into_boxed_str())),
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                ),
            });
        }
    }

    let name = if !attr.is_empty() {
        let name = syn::parse2::<LitStr>(attr)?;
        name.value().into_boxed_str()
    } else {
        input
            .ident
            .to_string()
            .to_case(convert_case::Case::Snake)
            .into_boxed_str()
    };

    let syntax = DynamicEffectSyntax {
        name: Box::leak(name),
        fields: Box::leak(field_syntaxes.into_boxed_slice()),
        is_material,
    };

    let doc_string = syntax.doc_fmt();
    let doc_attribute: Attribute = syn::parse_quote! { #[doc = #doc_string] };
    input.attrs.push(doc_attribute);

    let ident = &input.ident;
    Ok(quote! {
        #input

        // derive default from the syntax values for soundness
        #[automatically_derived]
        impl Default for #ident {
            fn default() -> Self {
                Self {
                    #(#default_fields,)*
                }
            }
        }

        impl pretty_text::dynamic_effects::syntax::GetDynamicEffectSyntax for #ident {
            fn get(&self) -> pretty_text::dynamic_effects::syntax::DynamicEffectSyntax<'static> {
                #syntax
            }
        }
    })
}
