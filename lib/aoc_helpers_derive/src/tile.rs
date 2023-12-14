use proc_macro2::{Span, TokenStream};
use quote::quote;

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
    variants: Vec<TileEnumVariant<'a>>,
}

#[derive(Debug)]
struct TileEnumVariant<'a> {
    ident: &'a syn::Ident,
    symbol: syn::LitChar,
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

fn parse_enum_variant(variant: &syn::Variant) -> Result<TileEnumVariant<'_>> {
    let mut tile_symbol = None;
    for attr in &variant.attrs {
        if let Some(symbol) = parse_tile_attribute(attr)? {
            tile_symbol = Some(symbol);
        }
    }
    let Some(symbol) = tile_symbol else {
        return Err(Error::new_spanned(variant, "Missing `#[tile()]` attribute"));
    };

    Ok(TileEnumVariant {
        ident: &variant.ident,
        symbol,
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
///     }
/// }
/// ```
fn generate_symbol_method(eenum: &TileEnum) -> TokenStream {
    let match_arms: Vec<_> = eenum
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let symbol = &variant.symbol;
            quote! {
                Self::#ident => #symbol as u8
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

    let match_arms: Vec<_> = eenum
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let symbol = &variant.symbol;
            quote! {
                #symbol => Ok(Self::#ident),
            }
        })
        .collect();

    quote! {
        impl ::core::convert::From<#ident> for char {
            fn from(data: #ident) -> Self {
                #ident::symbol(&data) as char
            }
        }

        impl ::core::convert::TryFrom<char> for #ident {
            type Error = ();

            fn try_from(data: char) -> ::core::result::Result<Self, Self::Error> {
                match data {
                    #(#match_arms)*
                    _ => Err(()),
                }
            }
        }
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
            let symbol = &variant.symbol;
            quote! {
                ::nom::Parser::map(
                    ::nom::character::complete::char(#symbol),
                    |_| Self::#ident
                )
            }
        })
        .collect();

    quote! {
        fn parse(input: &str) -> ::nom::IResult<&str, Self> {
            ::nom::branch::alt((
                #(#variant_parsers),*
            ))(input)
        }
    }
}
