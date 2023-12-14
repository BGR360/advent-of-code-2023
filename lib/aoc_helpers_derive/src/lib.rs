use syn::DeriveInput;

mod tile;

type Error = syn::Error;
type Result<T> = syn::Result<T>;

#[proc_macro_derive(Tile, attributes(tile))]
pub fn my_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);

    tile::generate(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
