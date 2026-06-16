// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use once_cell::sync::Lazy as SyncLazy;

use crate::constraints::{add_where_clauses, TokenStreamWithConstraints};
use crate::encoding::*;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_quote, punctuated::Punctuated, spanned::Spanned};

const NOM_TUPLE_MAX: usize = 26;

pub fn generate_nom_read_for_data(
    generics: &syn::Generics,
    data: &DataWithEncoding,
) -> TokenStream {
    let name = data.name;
    let TokenStreamWithConstraints {
        stream: nom_read,
        constraints,
    } = generate_nom_read(&data.encoding);
    // We want to derive NomReader<'a> for a fresh 'a.  To do this we
    // use a mix of the solutions proposed in
    // https://github.com/dtolnay/syn/issues/90
    let a: syn::GenericParam = parse_quote!('_a);
    let mut extended_generics = generics.clone();
    extended_generics.params.push(a.clone());
    let (impl_generics, _, _) = extended_generics.split_for_impl();
    let (_, ty_generics, where_clause) = generics.split_for_impl();
    let where_clause = add_where_clauses(where_clause.cloned(), constraints, data.name.span());
    quote_spanned! {
        data.name.span()=>
        #[allow(unused_parens)]
        #[allow(clippy::unnecessary_cast)]
        #[allow(clippy::redundant_closure_call)]
        impl #impl_generics tezos_data_encoding::nom::NomReader<#a> for #name #ty_generics #where_clause {
            fn nom_read(bytes: &#a [u8]) -> tezos_data_encoding::nom::NomResult<#a, Self> {
                #nom_read(bytes)
            }
        }
    }
}

fn generate_nom_read(encoding: &Encoding) -> TokenStreamWithConstraints {
    match encoding {
        Encoding::Unit(span) => generate_unit_nom_read(*span).into(),
        Encoding::Primitive(primitive, span) => {
            generate_primitive_nom_read(*primitive, *span).into()
        }
        Encoding::Bytes(span) => generate_bytes_nom_read(*span).into(),
        Encoding::Path(path) => {
            let mut constraints = Punctuated::new();
            constraints
                .push(parse_quote!(#path : for <'a> tezos_data_encoding::nom::NomReader<'a>));
            TokenStreamWithConstraints {
                stream: quote_spanned!(path.span()=><#path as tezos_data_encoding::nom::NomReader>::nom_read),
                constraints,
            }
        }
        Encoding::Struct(encoding) => generate_struct_nom_read(encoding),
        Encoding::Enum(encoding) => generate_enum_nom_read(encoding),
        Encoding::String(size, span) => generate_string_nom_read(size, *span).into(),
        Encoding::OptionField(encoding, span) => generate_optional_field_nom_read(encoding, *span),
        Encoding::List(size, encoding, span) => generate_list_nom_read(size, encoding, *span),
        Encoding::Sized(size, encoding, span) => generate_sized_nom_read(size, encoding, *span),
        Encoding::Bounded(size, encoding, span) => generate_bounded_nom_read(size, encoding, *span),
        Encoding::ShortDynamic(encoding, span) => generate_short_dynamic_nom_read(encoding, *span),
        Encoding::Dynamic(size, encoding, span) => generate_dynamic_nom_read(size, encoding, *span),
        Encoding::Zarith(span) => quote_spanned!(*span=> tezos_data_encoding::nom::zarith).into(),
        Encoding::Narith(span) => quote_spanned!(*span=> tezos_data_encoding::nom::narith).into(),
    }
}

fn get_primitive_byte_mapping(kind: PrimitiveEncoding) -> Option<&'static str> {
    static PRIMITIVE_BYTES_MAPPING: SyncLazy<Vec<(PrimitiveEncoding, &'static str)>> =
        SyncLazy::new(|| {
            use crate::encoding::PrimitiveEncoding::*;
            vec![(Int8, "i8"), (Uint8, "u8")]
        });
    PRIMITIVE_BYTES_MAPPING
        .iter()
        .find_map(|(k, s)| if kind == *k { Some(*s) } else { None })
}

fn generate_unit_nom_read(span: Span) -> TokenStream {
    quote_spanned!(span=> tezos_data_encoding::nom::unit)
}

fn generate_primitive_nom_read(kind: PrimitiveEncoding, span: Span) -> TokenStream {
    match kind {
        PrimitiveEncoding::Int8 | PrimitiveEncoding::Uint8 => {
            generate_byte_nom_read(get_primitive_byte_mapping(kind).unwrap(), span)
        }
        PrimitiveEncoding::Int16
        | PrimitiveEncoding::Uint16
        | PrimitiveEncoding::Int31
        | PrimitiveEncoding::Int32
        | PrimitiveEncoding::Uint32
        | PrimitiveEncoding::Int64
        | PrimitiveEncoding::Float
        | PrimitiveEncoding::Timestamp => {
            generate_number_nom_read(get_primitive_number_mapping(kind).unwrap(), span)
        }
        PrimitiveEncoding::Bool => quote_spanned!(span=> tezos_data_encoding::nom::boolean),
    }
}

fn generate_byte_nom_read(num: &str, span: Span) -> TokenStream {
    let ty = syn::Ident::new(num, span);
    quote_spanned!(span=> nom::number::complete::#ty)
}

fn generate_number_nom_read(num: &str, span: Span) -> TokenStream {
    let ty = syn::Ident::new(num, span);
    quote_spanned!(span=> nom::number::complete::#ty(nom::number::Endianness::Big))
}

fn generate_bytes_nom_read(span: Span) -> TokenStream {
    quote_spanned!(span=> tezos_data_encoding::nom::bytes)
}

fn generate_struct_nom_read(encoding: &StructEncoding) -> TokenStreamWithConstraints {
    let generate_nom_read = match encoding.fields.len() {
        0 => unreachable!("No decoding for empty struct"),
        1 => generate_struct_one_field_nom_read,
        n if n < NOM_TUPLE_MAX => generate_struct_many_fields_nom_read,
        _ => generate_struct_multi_fields_nom_read,
    };
    generate_nom_read(encoding)
}

fn generate_struct_one_field_nom_read(encoding: &StructEncoding) -> TokenStreamWithConstraints {
    let name = encoding.name;
    let field = encoding.fields.first().unwrap();
    let field_name = field.name;
    let field_name_str = field_name.to_string();
    generate_struct_field_nom_read(field).map_stream(|field_nom_read|
    quote_spanned!(encoding.name.span()=> nom::combinator::map(tezos_data_encoding::nom::field(#field_name_str, #field_nom_read), |#field_name| #name { #field_name })))
}

fn generate_struct_many_fields_nom_read(encoding: &StructEncoding) -> TokenStreamWithConstraints {
    let name = encoding.name;
    let (fields, hash) = encoding
        .fields
        .iter()
        .partition::<Vec<_>, _>(|f| !matches!(f.kind, FieldKind::Hash));

    let field1 = fields.iter().map(|field| field.name);

    let field2 = field1.clone();
    let field_name = fields
        .iter()
        .map(|field| format!("{}::{}", name, field.name));
    let mut constraints = Punctuated::new();
    let field_nom_read = encoding.fields.iter().map(|field| {
        let TokenStreamWithConstraints {
            stream,
            constraints: field_constraints,
        } = generate_struct_field_nom_read(field);
        constraints.extend(field_constraints);
        stream
    });
    let stream = if let Some(hash_field) = hash.first() {
        let field3 = field1.clone();
        let hash_name = hash_field.name;
        quote_spanned! {
            hash_field.name.span()=>
                nom::combinator::map(
                    tezos_data_encoding::nom::hashed(
                        tezos_crypto_rs::hash::TezosHasher,
                        nom::sequence::tuple((
                            #(tezos_data_encoding::nom::field(#field_name, #field_nom_read)),*
                        ))
                    ),
                    |((#(#field2),*), #hash_name)| {
                        #name { #(#field3),*, #hash_name: #hash_name.into() }
                    })
        }
    } else {
        quote_spanned! {
            encoding.name.span()=>
                nom::combinator::map(
                    nom::sequence::tuple((
                        #(tezos_data_encoding::nom::field(#field_name, #field_nom_read)),*
                    )),
                    |(#(#field1),*)| #name { #(#field2),* }
                )
        }
    };
    TokenStreamWithConstraints {
        stream,
        constraints,
    }
}

fn generate_struct_multi_fields_nom_read(encoding: &StructEncoding) -> TokenStreamWithConstraints {
    let name = encoding.name;
    let (fields, hash) = encoding
        .fields
        .iter()
        .partition::<Vec<_>, _>(|f| !matches!(f.kind, FieldKind::Hash));
    let field1 = fields.iter().map(|field| field.name);
    let field2 = field1.clone();
    let field_name = fields
        .iter()
        .map(|field| format!("{}::{}", name, field.name));
    let mut constraints = Punctuated::new();
    let field_nom_read = encoding.fields.iter().map(|field| {
        let TokenStreamWithConstraints {
            stream,
            constraints: field_constraints,
        } = generate_struct_field_nom_read(field);
        constraints.extend(field_constraints);
        stream
    });
    let stream = if let Some(hash_field) = hash.first() {
        let field3 = field1.clone();
        let field4 = field1.clone();
        let hash_name = hash_field.name;
        quote_spanned! {
            hash_field.name.span()=>
                nom::combinator::map(
                    tezos_data_encoding::nom::hashed(
                        tezos_crypto_rs::hash::TezosHasher,
                        (|input| {
                            #(let (input, #field1) = tezos_data_encoding::nom::field(#field_name, #field_nom_read)(input)?;)*
                            Ok((input, (#(#field2),* )))
                        })
                    ),
                    |((#(#field3),*), #hash_name)| {
                        #name { #(#field4),*, #hash_name: #hash_name.into() }
                    }
                )
        }
    } else {
        quote_spanned! {
            encoding.name.span()=>
                (|input| {
                    #(let (input, #field1) = tezos_data_encoding::nom::field(#field_name, #field_nom_read)(input)?;)*
                    Ok((input, #name { #(#field2),* }))
                })
        }
    };
    TokenStreamWithConstraints {
        stream,
        constraints,
    }
}

fn generate_struct_field_nom_read(field: &FieldEncoding) -> TokenStreamWithConstraints {
    match field.kind {
        FieldKind::Encoded(ref field_enc) => {
            generate_nom_read(&field_enc.encoding).map_stream(|encoding| {
                if let Some(ref reserve) = field_enc.reserve {
                    quote! {
                        tezos_data_encoding::nom::reserve(
                            #reserve,
                            #encoding
                        )
                    }
                } else {
                    encoding
                }
            })
        }
        FieldKind::Skip => quote!(|input| Ok((input, Default::default()))).into(),
        FieldKind::Hash => unreachable!(),
    }
}

fn generate_enum_nom_read(encoding: &EnumEncoding) -> TokenStreamWithConstraints {
    let tag_type = &encoding.tag_type;
    let tag_read = if encoding.tag_type == crate::symbol::rust::U8 {
        quote_spanned!(encoding.tag_type.span()=> nom::number::complete::u8)
    } else {
        quote_spanned!(encoding.tag_type.span()=> nom::number::complete::#tag_type(nom::number::Endianness::Big))
    };
    let tag_id = encoding.tags.iter().map(|tag| tag.id.clone());
    let mut constraints = Punctuated::new();
    let tags_nom_read = encoding.tags.iter().map(|tag| {
        let TokenStreamWithConstraints {
            stream,
            constraints: tag_constraints,
        } = generate_tag_nom_read(tag, encoding.name);
        constraints.extend(tag_constraints);
        stream
    });
    let unknown_tag_error = if encoding.ignore_unknown {
        "unknown_tag"
    } else {
        "invalid_tag"
    };
    let unknown_tag_error = format_ident!("{}", unknown_tag_error, span = tag_type.span());
    TokenStreamWithConstraints {
        stream: quote_spanned! {
        tag_type.span()=>
            (|input| {
                let (input, tag) = #tag_read(input)?;
                let (input, variant) = #(
                    if tag == #tag_id {
                        (#tags_nom_read)(input)?
                    } else
                )*
                {
                    return Err(
                        nom::Err::Error(
                            tezos_data_encoding::nom::error::DecodeError::#unknown_tag_error(
                                input,
                                format!("0x{:.2X}", tag)
                            )
                        )
                    );
                };
                Ok((input, variant))
            })
        },
        constraints,
    }
}

fn generate_tag_nom_read(tag: &Tag<'_>, enum_name: &syn::Ident) -> TokenStreamWithConstraints {
    let tag_name = tag.name;
    match &tag.encoding {
        Encoding::Unit(span) => {
            quote_spanned!(*span=> |bytes| Ok((bytes, #enum_name::#tag_name))).into()
        }
        encoding => {
            generate_nom_read(encoding).map_stream(|nom_read| {
            let name = format!("{enum_name}::{tag_name}");
                quote_spanned!(tag_name.span()=> nom::combinator::map(tezos_data_encoding::nom::variant(#name, #nom_read), #enum_name::#tag_name))})
        }
    }
}

fn generate_string_nom_read(size: &Option<syn::Expr>, span: Span) -> TokenStream {
    size.as_ref().map_or_else(
        || quote_spanned!(span=> tezos_data_encoding::nom::string),
        |size| quote_spanned!(span=> tezos_data_encoding::nom::bounded_string(#size)),
    )
}

fn generate_optional_field_nom_read(encoding: &Encoding, span: Span) -> TokenStreamWithConstraints {
    generate_nom_read(encoding).map_stream(
        |nom_read| quote_spanned!(span=> tezos_data_encoding::nom::optional_field(#nom_read)),
    )
}

fn generate_list_nom_read(
    size: &Option<syn::Expr>,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_nom_read(encoding).map_stream(|nom_read| {
        size.as_ref().map_or_else(
            || quote_spanned!(span=> tezos_data_encoding::nom::list(#nom_read)),
            |size| quote_spanned!(span=> tezos_data_encoding::nom::bounded_list(#size, #nom_read)),
        )
    })
}

fn generate_sized_nom_read(
    size: &syn::Expr,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_nom_read(encoding).map_stream(
        |nom_read| quote_spanned!(span=> tezos_data_encoding::nom::sized(#size, #nom_read)),
    )
}

fn generate_bounded_nom_read(
    size: &syn::Expr,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_nom_read(encoding).map_stream(
        |nom_read| quote_spanned!(span=> tezos_data_encoding::nom::bounded(#size, #nom_read)),
    )
}

fn generate_short_dynamic_nom_read(encoding: &Encoding, span: Span) -> TokenStreamWithConstraints {
    generate_nom_read(encoding).map_stream(
        |nom_read| quote_spanned!(span=> tezos_data_encoding::nom::short_dynamic(#nom_read)),
    )
}

fn generate_dynamic_nom_read(
    size: &Option<syn::Expr>,
    encoding: &Encoding,
    span: Span,
) -> TokenStreamWithConstraints {
    generate_nom_read(encoding).map_stream(|nom_read| {
        size.as_ref().map_or_else(
        || quote_spanned!(span=> tezos_data_encoding::nom::dynamic(#nom_read)),
        |size| quote_spanned!(span=> tezos_data_encoding::nom::bounded_dynamic(#size, #nom_read)),
    )
    })
}
