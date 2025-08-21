// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-License-Identifier: MIT

use crate::constraints::{add_where_clauses, TokenStreamWithConstraints};
use crate::encoding::*;
use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;
use syn::{parse_quote, punctuated::Punctuated, spanned::Spanned};

pub fn generate_encoding_for_data(
    generics: &syn::Generics,
    data: &DataWithEncoding,
) -> TokenStream {
    let name = data.name;
    let TokenStreamWithConstraints {
        stream: encoding,
        constraints,
    } = generate_encoding(&data.encoding);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let where_clause = add_where_clauses(where_clause.cloned(), constraints, data.name.span());
    quote_spanned! {data.name.span()=>
        impl #impl_generics tezos_data_encoding::encoding::HasEncoding for #name #ty_generics #where_clause {
            fn encoding() -> tezos_data_encoding::encoding::Encoding {
                #encoding
            }
        }
    }
}

pub(crate) fn generate_encoding(encoding: &Encoding) -> TokenStreamWithConstraints {
    match encoding {
        Encoding::Unit(span) => {
            quote_spanned!(*span=> tezos_data_encoding::encoding::Encoding::Unit).into()
        }
        Encoding::Primitive(primitive, span) => {
            generate_primitive_encoding(*primitive, *span).into()
        }
        Encoding::Bytes(span) => {
            quote_spanned!(*span=> tezos_data_encoding::encoding::Encoding::Bytes).into()
        }
        Encoding::Path(path) => {
            let mut constraints = Punctuated::new();
            constraints.push(parse_quote!(#path : tezos_data_encoding::encoding::HasEncoding));
            TokenStreamWithConstraints {
                stream: quote_spanned!(path.span()=> #[allow(clippy::redundant_clone)]<#path as tezos_data_encoding::encoding::HasEncoding>::encoding().clone()),
                constraints,
            }
        }
        Encoding::String(size, span) => generate_string_encoding(size, *span).into(),
        Encoding::Struct(encoding) => generate_struct_encoding(encoding),
        Encoding::Enum(encoding) => generate_enum_encoding(encoding),
        Encoding::OptionField(encoding, span) => generate_optional_field_encoding(encoding, *span),
        Encoding::List(size, encoding, span) => generate_list_encoding(size, encoding, *span),
        Encoding::Sized(size, encoding, span) => generate_sized_encoding(size, encoding, *span),
        Encoding::Bounded(size, encoding, span) => generate_bounded_encoding(size, encoding, *span),
        Encoding::ShortDynamic(encoding, span) => generate_short_dynamic_encoding(encoding, *span),
        Encoding::Dynamic(size, encoding, span) => generate_dynamic_encoding(size, encoding, *span),
        Encoding::Zarith(span) => {
            quote_spanned!(*span=> tezos_data_encoding::encoding::Encoding::Z).into()
        }
        Encoding::Narith(span) => {
            quote_spanned!(*span=> tezos_data_encoding::encoding::Encoding::N).into()
        }
    }
}

fn generate_primitive_encoding(kind: PrimitiveEncoding, span: Span) -> TokenStream {
    let ident = kind.make_ident(span);
    quote_spanned!(ident.span()=> tezos_data_encoding::encoding::Encoding::#ident)
}

fn generate_struct_encoding(encoding: &StructEncoding) -> TokenStreamWithConstraints {
    let name_str = encoding.name.to_string();
    let mut constraints = Punctuated::new();
    let fields_encoding = encoding.fields.iter().filter_map(|field| {
        generate_field_encoding(field).map(
            |TokenStreamWithConstraints {
                 stream,
                 constraints: field_constraints,
             }| {
                constraints.extend(field_constraints);
                stream
            },
        )
    });
    TokenStreamWithConstraints {
        stream: quote_spanned! { encoding.name.span()=>
            tezos_data_encoding::encoding::Encoding::Obj(#name_str, vec![
                #(#fields_encoding),*
            ])
        },
        constraints,
    }
}

fn generate_field_encoding(field: &FieldEncoding) -> Option<TokenStreamWithConstraints> {
    if let FieldKind::Encoded(encoding) = &field.kind {
        let name = field.name.to_string();
        Some(
            generate_encoding(&encoding.encoding).map_stream(|encoding|
            quote_spanned!(field.name.span()=> tezos_data_encoding::encoding::Field::new(#name, #encoding))),
        )
    } else {
        None
    }
}

fn generate_enum_encoding(encoding: &EnumEncoding) -> TokenStreamWithConstraints {
    let tag_type = &encoding.tag_type;
    let mut constraints = Punctuated::new();
    let tags_encoding = encoding.tags.iter().map(|tag| {
        let TokenStreamWithConstraints {
            stream,
            constraints: tag_constraints,
        } = generate_tag_encoding(tag);
        constraints.extend(tag_constraints);
        stream
    });
    TokenStreamWithConstraints {
        stream: quote_spanned! { tag_type.span()=>
        tezos_data_encoding::encoding::Encoding::Tags(
            std::mem::size_of::<#tag_type>(),
            tezos_data_encoding::encoding::TagMap::new(vec![
                #(#tags_encoding),*
            ])
        )
        },
        constraints,
    }
}

fn generate_tag_encoding(tag: &Tag) -> TokenStreamWithConstraints {
    let id = &tag.id;
    let name = tag.name.to_string();
    generate_encoding(&tag.encoding).map_stream(|encoding|
    quote_spanned!(tag.name.span()=> tezos_data_encoding::encoding::Tag::new(#id, #name, #encoding)))
}

fn generate_string_encoding(size: &Option<syn::Expr>, span: Span) -> TokenStream {
    size.as_ref().map_or_else(
        || quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::String),
        |size| quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::BoundedString(#size)),
    )
}

fn generate_list_encoding(
    size: &Option<syn::Expr>,
    encoding: &Encoding<'_>,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_encoding(encoding).map_stream(|encoding|
    size.as_ref().map_or_else(|| quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::List(Box::new(#encoding))), |size| quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::BoundedList(#size, Box::new(#encoding)))))
}

fn generate_optional_field_encoding(encoding: &Encoding, span: Span) -> TokenStreamWithConstraints {
    generate_encoding(encoding).map_stream(|encoding|
    quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::OptionalField(Box::new(#encoding))))
}

fn generate_sized_encoding(
    size: &syn::Expr,
    encoding: &Encoding<'_>,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_encoding(encoding).map_stream(|encoding|
    quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::Sized(#size, Box::new(#encoding))))
}

fn generate_bounded_encoding(
    size: &syn::Expr,
    encoding: &Encoding<'_>,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_encoding(encoding).map_stream(|encoding|
    quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::Bounded(#size, Box::new(#encoding))))
}

fn generate_short_dynamic_encoding(encoding: &Encoding, span: Span) -> TokenStreamWithConstraints {
    generate_encoding(encoding).map_stream(|encoding|
    quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::ShortDynamic(Box::new(#encoding))))
}

fn generate_dynamic_encoding(
    size: &Option<syn::Expr>,
    encoding: &Encoding<'_>,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_encoding(encoding).map_stream(|encoding|
    size.as_ref().map_or_else(
        || quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::Dynamic(Box::new(#encoding))),
        |size| quote_spanned!(span=> tezos_data_encoding::encoding::Encoding::BoundedDynamic(#size, Box::new(#encoding)))))
}
