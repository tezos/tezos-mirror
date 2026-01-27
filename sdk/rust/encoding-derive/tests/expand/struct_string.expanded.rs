use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructString {
    a: String,
    #[encoding(string = "32")]
    b: String,
}
impl tezos_data_encoding::encoding::HasEncoding for StructString {
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructString",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a",
                        tezos_data_encoding::encoding::Encoding::String,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "b",
                        tezos_data_encoding::encoding::Encoding::BoundedString(32),
                    ),
                ]),
            ),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for StructString {
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "StructString::a",
                    tezos_data_encoding::nom::string,
                ),
                tezos_data_encoding::nom::field(
                    "StructString::b",
                    tezos_data_encoding::nom::bounded_string(32),
                ),
            )),
            |(a, b)| StructString { a, b },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for StructString {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructString::a",
                tezos_data_encoding::enc::string,
            )(&data.a, out)?;
            tezos_data_encoding::enc::field(
                "StructString::b",
                tezos_data_encoding::enc::bounded_string(32),
            )(&data.b, out)?;
            Ok(())
        })(self, out)
    }
}
