use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

use crate::{Error, Result};

pub fn generate(input: syn::DeriveInput) -> Result<TokenStream> {
    //eprintln!("{input:#?}");

    let eenum = parse_enum(&input)?;

    let ident = &eenum.ident;

    let symbol_method = generate_symbol_method(&eenum);
    let parse_method = generate_nom_parse(&eenum);

    let u8_conversion = generate_u8_conversion(&eenum);
    let char_conversion = generate_char_conversion(&eenum);
    let from_str = generate_from_str(&eenum);
    let display = generate_display(&eenum);

    Ok(quote! {
        impl #ident {
            #symbol_method
            #parse_method
        }

        #u8_conversion
        #char_conversion
        #from_str
        #display
    })
}

#[derive(Debug)]
struct TileEnum<'a> {
    ident: &'a syn::Ident,
    variants: Vec<TileVariant<'a>>,
}

#[derive(Debug)]
struct TileVariant<'a> {
    ident: &'a syn::Ident,
    kind: TileVariantKind<'a>,
}

#[derive(Debug)]
enum TileVariantKind<'a> {
    Unit { symbol: syn::LitChar },
    Tuple { ty: &'a syn::Type },
}

fn parse_enum(input: &syn::DeriveInput) -> Result<TileEnum<'_>> {
    let ident = &input.ident;

    let eenum = match &input.data {
        syn::Data::Enum(e) => e,
        _ => {
            return Err(Error::new(
                Span::call_site(),
                "`Tile` can only be derived for `enum`s",
            ))
        }
    };

    let variants = eenum
        .variants
        .iter()
        .map(parse_enum_variant)
        .collect::<Result<_>>()?;

    Ok(TileEnum { ident, variants })
}

fn parse_enum_variant(variant: &syn::Variant) -> Result<TileVariant<'_>> {
    let mut tile_attr = None;
    let mut tile_symbol = None;
    for attr in &variant.attrs {
        if let Some(symbol) = parse_tile_attribute(attr)? {
            tile_symbol = Some(symbol);
            tile_attr = Some(attr);
        }
    }

    let kind = match (tile_symbol, &variant.fields) {
        (Some(symbol), syn::Fields::Unit) => TileVariantKind::Unit { symbol },
        (None, syn::Fields::Unnamed(fields)) if fields.unnamed.len() == 1 => {
            let field = fields.unnamed.first().unwrap();
            TileVariantKind::Tuple { ty: &field.ty }
        }

        (None, syn::Fields::Unit) => {
            return Err(Error::new_spanned(
                variant,
                "missing `#[tile()]` attribute on variant",
            ));
        }
        (Some(_), syn::Fields::Unnamed(_)) => {
            return Err(Error::new_spanned(
                tile_attr.unwrap(),
                "`#[tile()]` attribute is not supported on tuple variants",
            ));
        }

        (_, syn::Fields::Unnamed(fields)) => {
            return Err(Error::new_spanned(
                fields,
                "tuple variants must have exactly 1 field",
            ));
        }
        (_, syn::Fields::Named(_)) => {
            return Err(Error::new_spanned(
                variant,
                "struct variants are not supported",
            ));
        }
    };

    Ok(TileVariant {
        ident: &variant.ident,
        kind,
    })
}

/// Extracts the variant's symbol from an attribute like `#[tile('.')]`.
fn parse_tile_attribute(attr: &syn::Attribute) -> Result<Option<syn::LitChar>> {
    let path_is_ident =
        |path: &syn::Path, ident: &str| path.get_ident().map(|i| i == ident).unwrap_or(false);

    // Ignore attributes that aren't `#[tile()]`.
    let tokens = match &attr.meta {
        syn::Meta::List(syn::MetaList { path, tokens, .. }) if path_is_ident(path, "tile") => {
            tokens
        }
        _ => return Ok(None),
    };

    let lit_char: syn::LitChar = syn::parse2(tokens.clone())?;
    if !lit_char.value().is_ascii() {
        return Err(Error::new_spanned(
            tokens,
            "Symbol must be an ASCII character",
        ));
    }

    // let symbol: char = lit_char.value();
    // let symbol = u8::try_from(symbol)
    //     .map_err(|_| Error::new_spanned(tokens, "Symbol must be an ASCII character"))?;

    Ok(Some(lit_char))
}

/// Generates the `symbol()` method for the derive type:
///
/// ```rust,no_run
/// fn symbol(&self) -> u8 {
///     match self {
///         Self::A => b'a',
///         Self::B => b'b',
///         Self::Other(inner) => inner.symbol(),
///     }
/// }
/// ```
fn generate_symbol_method(eenum: &TileEnum) -> TokenStream {
    let match_arms: Vec<_> = eenum
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let span = ident.span();
            match &variant.kind {
                TileVariantKind::Unit { symbol } => quote_spanned! {span=>
                    Self::#ident => u8::try_from(#symbol).unwrap()
                },
                TileVariantKind::Tuple { ty } => quote_spanned! {span=>
                    Self::#ident(inner) => #ty::symbol(inner)
                },
            }
        })
        .collect();

    quote! {
        pub fn symbol(&self) -> u8 {
            match self {
                #(#match_arms),*
            }
        }
    }
}

/// Generates `From<Tile> for u8` and `TryFrom<u8> for Tile`.
fn generate_u8_conversion(eenum: &TileEnum) -> TokenStream {
    let ident = &eenum.ident;

    quote! {
        impl ::core::convert::From<#ident> for u8 {
            fn from(data: #ident) -> Self {
                #ident::symbol(&data)
            }
        }

        impl ::core::convert::TryFrom<u8> for #ident {
            type Error = ();

            fn try_from(data: u8) -> ::core::result::Result<Self, Self::Error> {
                ::core::convert::TryFrom::try_from(data as char)
            }
        }
    }
}

/// Generates `From<Tile> for char` and `TryFrom<char> for Tile`.
fn generate_char_conversion(eenum: &TileEnum) -> TokenStream {
    let ident = &eenum.ident;

    let to_char = quote! {
        impl ::core::convert::From<#ident> for char {
            fn from(data: #ident) -> Self {
                #ident::symbol(&data) as char
            }
        }
    };

    let unit_match_arms: Vec<_> = eenum
        .variants
        .iter()
        .filter_map(|variant| {
            let ident = &variant.ident;
            let span = ident.span();
            match &variant.kind {
                TileVariantKind::Unit { symbol } => Some(quote_spanned! {span=>
                    #symbol => Ok(Self::#ident),
                }),
                _ => None,
            }
        })
        .collect();

    let tuple_try_froms: Vec<_> = eenum
        .variants
        .iter()
        .filter_map(|variant| {
            let ident = &variant.ident;
            let span = ident.span();
            match &variant.kind {
                TileVariantKind::Tuple { ty } => Some(quote_spanned! {span=>
                    match <#ty as ::core::convert::TryFrom<char>>::try_from(data) {
                        Ok(inner) => return Ok(Self::#ident(inner)),
                        Err(_) => {}
                    }
                }),
                _ => None,
            }
        })
        .collect();

    let from_char = quote! {
        impl ::core::convert::TryFrom<char> for #ident {
            type Error = ();

            fn try_from(data: char) -> ::core::result::Result<Self, Self::Error> {
                match data {
                    #(#unit_match_arms)*
                    _ => {
                        #(#tuple_try_froms)*
                        Err(())
                    }
                }
            }
        }
    };

    quote! {
        #to_char

        #from_char
    }
}

/// Generates `impl FromStr for Tile`.
fn generate_from_str(eenum: &TileEnum) -> TokenStream {
    let ident = &eenum.ident;

    quote! {
        impl ::core::str::FromStr for #ident {
            type Err = ();

            fn from_str(s: &str) -> ::core::result::Result<Self, Self::Err> {
                match s.as_bytes()[..] {
                    [b] => ::core::convert::TryFrom::try_from(b),
                    _ => Err(()),
                }
            }
        }
    }
}

/// Generates `impl Display for Tile`.
fn generate_display(eenum: &TileEnum) -> TokenStream {
    let ident = &eenum.ident;

    quote! {
        impl ::core::fmt::Display for #ident {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                let symbol = #ident::symbol(self) as char;
                ::core::fmt::Display::fmt(&symbol, f)
            }
        }
    }
}

fn generate_nom_parse(eenum: &TileEnum) -> TokenStream {
    let variant_parsers: Vec<_> = eenum
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let span = ident.span();
            match &variant.kind {
                TileVariantKind::Unit { symbol } => quote_spanned! {span=>
                    ::nom::Parser::map(
                        ::nom::character::complete::char(#symbol),
                        |_| Self::#ident
                    )
                },
                TileVariantKind::Tuple { ty } => quote_spanned! {span=>
                    ::nom::Parser::map(
                        #ty::parse,
                        Self::#ident
                    )
                },
            }
        })
        .collect();

    quote! {
        pub fn parse(input: &str) -> ::nom::IResult<&str, Self> {
            ::nom::branch::alt((
                #(#variant_parsers),*
            ))(input)
        }
    }
}
