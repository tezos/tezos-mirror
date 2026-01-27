use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructTimestamp {
    a: i64,
    #[encoding(timestamp)]
    b: i64,
}
impl tezos_data_encoding::encoding::HasEncoding for StructTimestamp {
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructTimestamp",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a",
                        tezos_data_encoding::encoding::Encoding::Int64,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "b",
                        tezos_data_encoding::encoding::Encoding::Timestamp,
                    ),
                ]),
            ),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for StructTimestamp {
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "StructTimestamp::a",
                    nom::number::complete::i64(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructTimestamp::b",
                    nom::number::complete::i64(nom::number::Endianness::Big),
                ),
            )),
            |(a, b)| StructTimestamp { a, b },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for StructTimestamp {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructTimestamp::a",
                tezos_data_encoding::enc::i64,
            )(&data.a, out)?;
            tezos_data_encoding::enc::field(
                "StructTimestamp::b",
                tezos_data_encoding::enc::i64,
            )(&data.b, out)?;
            Ok(())
        })(self, out)
    }
}
