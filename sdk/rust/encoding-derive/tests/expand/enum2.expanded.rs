use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
enum Enum2 {
    A(A),
    B,
}
impl tezos_data_encoding::encoding::HasEncoding for Enum2
where
    A: tezos_data_encoding::encoding::HasEncoding,
{
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Tags(
            std::mem::size_of::<u8>(),
            tezos_data_encoding::encoding::TagMap::new(
                <[_]>::into_vec(
                    ::alloc::boxed::box_new([
                        tezos_data_encoding::encoding::Tag::new(
                            0,
                            "A",
                            #[allow(clippy::redundant_clone)]
                            <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                .clone(),
                        ),
                        tezos_data_encoding::encoding::Tag::new(
                            1,
                            "B",
                            tezos_data_encoding::encoding::Encoding::Unit,
                        ),
                    ]),
                ),
            ),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for Enum2
where
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
{
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        (|input| {
            let (input, tag) = nom::number::complete::u8(input)?;
            let (input, variant) = if tag == 0 {
                (nom::combinator::map(
                    tezos_data_encoding::nom::variant(
                        "Enum2::A",
                        <A as tezos_data_encoding::nom::NomReader>::nom_read,
                    ),
                    Enum2::A,
                ))(input)?
            } else if tag == 1 {
                (|bytes| Ok((bytes, Enum2::B)))(input)?
            } else {
                return Err(
                    nom::Err::Error(
                        tezos_data_encoding::nom::error::DecodeError::invalid_tag(
                            input,
                            ::alloc::__export::must_use({
                                ::alloc::fmt::format(format_args!("0x{0:.2X}", tag))
                            }),
                        ),
                    ),
                );
            };
            Ok((input, variant))
        })(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for Enum2
where
    A: tezos_data_encoding::enc::BinWriter,
{
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out| {
            match data {
                Enum2::A(inner) => {
                    tezos_data_encoding::enc::variant_with_field(
                        "Enum2::A",
                        tezos_data_encoding::enc::u8,
                        <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                    )(&0, inner, out)
                }
                Enum2::B => {
                    tezos_data_encoding::enc::variant(
                        "Enum2::B",
                        tezos_data_encoding::enc::u8,
                    )(&1, out)
                }
            }
        })(self, out)
    }
}
