use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct Struct3 {
    a: A,
    b: B,
    c: C,
}
impl tezos_data_encoding::encoding::HasEncoding for Struct3
where
    A: tezos_data_encoding::encoding::HasEncoding,
    B: tezos_data_encoding::encoding::HasEncoding,
    C: tezos_data_encoding::encoding::HasEncoding,
{
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "Struct3",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a",
                        #[allow(clippy::redundant_clone)]
                        <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                            .clone(),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "b",
                        #[allow(clippy::redundant_clone)]
                        <B as tezos_data_encoding::encoding::HasEncoding>::encoding()
                            .clone(),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "c",
                        #[allow(clippy::redundant_clone)]
                        <C as tezos_data_encoding::encoding::HasEncoding>::encoding()
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
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for Struct3
where
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    B: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    C: for<'a> tezos_data_encoding::nom::NomReader<'a>,
{
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "Struct3::a",
                    <A as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
                tezos_data_encoding::nom::field(
                    "Struct3::b",
                    <B as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
                tezos_data_encoding::nom::field(
                    "Struct3::c",
                    <C as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
            )),
            |(a, b, c)| Struct3 { a, b, c },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for Struct3
where
    A: tezos_data_encoding::enc::BinWriter,
    B: tezos_data_encoding::enc::BinWriter,
    C: tezos_data_encoding::enc::BinWriter,
{
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "Struct3::a",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.a, out)?;
            tezos_data_encoding::enc::field(
                "Struct3::b",
                <B as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.b, out)?;
            tezos_data_encoding::enc::field(
                "Struct3::c",
                <C as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.c, out)?;
            Ok(())
        })(self, out)
    }
}
