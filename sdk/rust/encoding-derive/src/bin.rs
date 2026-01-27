// Copyright (c) SimpleStaking and Tezedge Contributors
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-License-Identifier: MIT

use crate::constraints::{add_where_clauses, TokenStreamWithConstraints};
use crate::encoding::*;
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use syn::{parse_quote, punctuated::Punctuated, spanned::Spanned};

pub fn generate_bin_write_for_data(
    generics: &syn::Generics,
    data: &DataWithEncoding,
) -> TokenStream {
    let name = data.name;
    let TokenStreamWithConstraints {
        stream: bin_write,
        constraints,
    } = generate_bin_write(&data.encoding);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let where_clause = add_where_clauses(where_clause.cloned(), constraints, data.name.span());
    quote_spanned! {
        data.name.span()=>
        #[allow(unused_parens)]
        #[allow(clippy::unnecessary_cast)]
        #[allow(clippy::redundant_closure_call)]
        impl #impl_generics tezos_data_encoding::enc::BinWriter for #name #ty_generics #where_clause {
            fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
                (#bin_write)(self, out)
            }
        }
    }
}

fn generate_bin_write(encoding: &Encoding) -> TokenStreamWithConstraints {
    match encoding {
        Encoding::Unit(span) => generate_unit_bin_write(*span).into(),
        Encoding::Primitive(primitive, span) => {
            generate_primitive_bin_write(*primitive, *span).into()
        }
        Encoding::Bytes(span) => generate_bytes_bin_write(*span).into(),
        Encoding::Path(path) => {
            let mut constraints = Punctuated::new();
            constraints.push(parse_quote!(#path : tezos_data_encoding::enc::BinWriter));
            TokenStreamWithConstraints {
                stream: quote_spanned!(path.span()=> <#path as tezos_data_encoding::enc::BinWriter>::bin_write),
                constraints,
            }
        }
        Encoding::Struct(encoding) => generate_struct_bin_write(encoding),
        Encoding::Enum(encoding) => generate_enum_bin_write(encoding),
        Encoding::String(size, span) => generate_string_bin_write(size, *span).into(),
        Encoding::OptionField(encoding, span) => generate_optional_field_bin_write(encoding, *span),
        Encoding::List(size, encoding, span) => generate_list_bin_write(size, encoding, *span),
        Encoding::Sized(size, encoding, span) => generate_sized_bin_write(size, encoding, *span),
        Encoding::Bounded(size, encoding, span) => {
            generate_bounded_bin_write(size, encoding, *span)
        }
        Encoding::ShortDynamic(encoding, span) => generate_short_dynamic_bin_write(encoding, *span),
        Encoding::Dynamic(size, encoding, span) => {
            generate_dynamic_bin_write(size, encoding, *span)
        }
        Encoding::Zarith(span) => quote_spanned!(*span=> tezos_data_encoding::enc::zarith).into(),
        Encoding::Narith(span) => quote_spanned!(*span=> tezos_data_encoding::enc::narith).into(),
    }
}

fn generate_bytes_bin_write(span: Span) -> TokenStream {
    quote_spanned!(span=> tezos_data_encoding::enc::bytes)
}

fn generate_unit_bin_write(span: Span) -> TokenStream {
    quote_spanned!(span=> tezos_data_encoding::enc::unit)
}

fn generate_primitive_bin_write(kind: PrimitiveEncoding, span: Span) -> TokenStream {
    match kind {
        PrimitiveEncoding::Int8
        | PrimitiveEncoding::Uint8
        | PrimitiveEncoding::Int16
        | PrimitiveEncoding::Uint16
        | PrimitiveEncoding::Int31
        | PrimitiveEncoding::Int32
        | PrimitiveEncoding::Uint32
        | PrimitiveEncoding::Int64
        | PrimitiveEncoding::Float
        | PrimitiveEncoding::Timestamp => {
            generate_number_bin_write(get_primitive_number_mapping(kind).unwrap(), span)
        }
        PrimitiveEncoding::Bool => quote_spanned!(span=> tezos_data_encoding::enc::boolean),
    }
}

fn generate_number_bin_write(num: &str, span: Span) -> TokenStream {
    let ty = syn::Ident::new(num, span);
    quote_spanned!(span=> tezos_data_encoding::enc::#ty)
}

fn generate_struct_bin_write(encoding: &StructEncoding) -> TokenStreamWithConstraints {
    let fields_with_encoding = encoding.fields.iter().filter(|f| f.encoding().is_some());
    let field = fields_with_encoding.clone().map(|f| f.name);
    let field_name = fields_with_encoding
        .clone()
        .map(|f| format!("{}::{}", encoding.name, f.name));
    let mut constraints = Punctuated::new();
    let field_bin_write = fields_with_encoding.map(|f| {
        let TokenStreamWithConstraints {
            stream,
            constraints: field_constraints,
        } = generate_struct_field_bin_write(f.encoding().unwrap());
        constraints.extend(field_constraints);
        stream
    });
    TokenStreamWithConstraints {
        stream: quote_spanned! {
            encoding.name.span()=>
                |data: &Self, out: &mut Vec<u8>| {
                    #(
                        tezos_data_encoding::enc::field(#field_name, #field_bin_write)(&data.#field, out)?;
                    )*
                    Ok(())
                }
        },
        constraints,
    }
}

fn generate_struct_field_bin_write(encoding: &Encoding) -> TokenStreamWithConstraints {
    generate_bin_write(encoding)
}

fn generate_enum_bin_write(encoding: &EnumEncoding) -> TokenStreamWithConstraints {
    let tag_type = &encoding.tag_type;
    let tag_serialize =
        quote_spanned!(encoding.tag_type.span()=> tezos_data_encoding::enc::#tag_type);
    let mut constraints = Punctuated::new();
    let tags_bin_write = encoding.tags.iter().map(|tag| {
        let TokenStreamWithConstraints {
            stream,
            constraints: tag_constraints,
        } = generate_tag_bin_write(tag, encoding.name, &tag_serialize);
        constraints.extend(tag_constraints);
        stream
    });
    TokenStreamWithConstraints {
        stream: quote_spanned! {
            tag_type.span()=>
                |data: &Self, out| {
                    match data {
                        #(#tags_bin_write),*
                    }
                }
        },
        constraints,
    }
}

fn generate_tag_bin_write(
    tag: &Tag<'_>,
    enum_name: &syn::Ident,
    tag_encoding: &TokenStream,
) -> TokenStreamWithConstraints {
    let tag_name = tag.name;
    let tag_id = &tag.id;
    let name = format!("{enum_name}::{tag_name}");
    match &tag.encoding {
        Encoding::Unit(span) => {
            quote_spanned!(*span=>
                           #enum_name::#tag_name => tezos_data_encoding::enc::variant(#name, #tag_encoding)(&#tag_id, out)
            ).into()
        }
        encoding => {
            generate_bin_write(encoding).map_stream(|bin_write|
                quote_spanned!(tag_name.span()=>
                           #enum_name::#tag_name(inner) => tezos_data_encoding::enc::variant_with_field(#name, #tag_encoding, #bin_write)(&#tag_id, inner, out)
            ))
        }
    }
}

fn generate_string_bin_write(size: &Option<syn::Expr>, span: Span) -> TokenStream {
    size.as_ref().map_or_else(
        || quote_spanned!(span=> tezos_data_encoding::enc::string),
        |size| quote_spanned!(span=> tezos_data_encoding::enc::bounded_string(#size)),
    )
}

fn generate_optional_field_bin_write(
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_bin_write(encoding).map_stream(
        |bin_write| quote_spanned!(span=> tezos_data_encoding::enc::optional_field(#bin_write)),
    )
}

fn generate_list_bin_write(
    size: &Option<syn::Expr>,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_bin_write(encoding).map_stream(|bin_write| {
        size.as_ref().map_or_else(
            || quote_spanned!(span=> tezos_data_encoding::enc::list(#bin_write)),
            |size| quote_spanned!(span=> tezos_data_encoding::enc::bounded_list(#size, #bin_write)),
        )
    })
}

fn generate_sized_bin_write(
    size: &syn::Expr,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_bin_write(encoding).map_stream(
        |bin_write| quote_spanned!(span=> tezos_data_encoding::enc::sized(#size, #bin_write)),
    )
}

fn generate_bounded_bin_write(
    size: &syn::Expr,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_bin_write(encoding).map_stream(
        |bin_write| quote_spanned!(span=> tezos_data_encoding::enc::bounded(#size, #bin_write)),
    )
}

fn generate_short_dynamic_bin_write(encoding: &Encoding, span: Span) -> TokenStreamWithConstraints {
    generate_bin_write(encoding).map_stream(
        |bin_write| quote_spanned!(span=> tezos_data_encoding::enc::short_dynamic(#bin_write)),
    )
}

fn generate_dynamic_bin_write(
    size: &Option<syn::Expr>,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_bin_write(encoding).map_stream(|bin_write| {
        size.as_ref().map_or_else(
        || quote_spanned!(span=> tezos_data_encoding::enc::dynamic(#bin_write)),
        |size| quote_spanned!(span=> tezos_data_encoding::enc::bounded_dynamic(#size, #bin_write)),
    )
    })
}
