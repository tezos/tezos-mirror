use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct Struct1 {
    a: A,
}
impl tezos_data_encoding::encoding::HasEncoding for Struct1
where
    A: tezos_data_encoding::encoding::HasEncoding,
{
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "Struct1",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a",
                        #[allow(clippy::redundant_clone)]
                        <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                            .clone(),
                    ),
                ]),
            ),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for Struct1
where
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
{
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            tezos_data_encoding::nom::field(
                "a",
                <A as tezos_data_encoding::nom::NomReader>::nom_read,
            ),
            |a| Struct1 { a },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for Struct1
where
    A: tezos_data_encoding::enc::BinWriter,
{
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "Struct1::a",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.a, out)?;
            Ok(())
        })(self, out)
    }
}
