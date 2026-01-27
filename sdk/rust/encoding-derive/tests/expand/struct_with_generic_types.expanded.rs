use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructWithGenericTypes<A, D> {
    a: A,
    b: B,
    c: C,
}
impl<A, D> tezos_data_encoding::encoding::HasEncoding for StructWithGenericTypes<A, D>
where
    A: tezos_data_encoding::encoding::HasEncoding,
    B: tezos_data_encoding::encoding::HasEncoding,
    C: tezos_data_encoding::encoding::HasEncoding,
{
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructWithGenericTypes",
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
impl<'_a, A, D> tezos_data_encoding::nom::NomReader<'_a> for StructWithGenericTypes<A, D>
where
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    B: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    C: for<'a> tezos_data_encoding::nom::NomReader<'a>,
{
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "StructWithGenericTypes::a",
                    <A as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
                tezos_data_encoding::nom::field(
                    "StructWithGenericTypes::b",
                    <B as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
                tezos_data_encoding::nom::field(
                    "StructWithGenericTypes::c",
                    <C as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
            )),
            |(a, b, c)| StructWithGenericTypes { a, b, c },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<A, D> tezos_data_encoding::enc::BinWriter for StructWithGenericTypes<A, D>
where
    A: tezos_data_encoding::enc::BinWriter,
    B: tezos_data_encoding::enc::BinWriter,
    C: tezos_data_encoding::enc::BinWriter,
{
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructWithGenericTypes::a",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.a, out)?;
            tezos_data_encoding::enc::field(
                "StructWithGenericTypes::b",
                <B as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.b, out)?;
            tezos_data_encoding::enc::field(
                "StructWithGenericTypes::c",
                <C as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.c, out)?;
            Ok(())
        })(self, out)
    }
}
