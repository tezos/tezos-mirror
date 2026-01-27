use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructUnit {
    a: (),
}
impl tezos_data_encoding::encoding::HasEncoding for StructUnit {
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructUnit",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a",
                        tezos_data_encoding::encoding::Encoding::Unit,
                    ),
                ]),
            ),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for StructUnit {
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            tezos_data_encoding::nom::field("a", tezos_data_encoding::nom::unit),
            |a| StructUnit { a },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for StructUnit {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructUnit::a",
                tezos_data_encoding::enc::unit,
            )(&data.a, out)?;
            Ok(())
        })(self, out)
    }
}
