use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructWithDuplicatedConstraint {
    a1: A,
    a2: A,
}
impl tezos_data_encoding::encoding::HasEncoding for StructWithDuplicatedConstraint
where
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
{
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructWithDuplicatedConstraint",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a1",
                        #[allow(clippy::redundant_clone)]
                        <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                            .clone(),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "a2",
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
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for StructWithDuplicatedConstraint
where
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
{
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "StructWithDuplicatedConstraint::a1",
                    <A as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
                tezos_data_encoding::nom::field(
                    "StructWithDuplicatedConstraint::a2",
                    <A as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
            )),
            |(a1, a2)| StructWithDuplicatedConstraint {
                a1,
                a2,
            },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for StructWithDuplicatedConstraint
where
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
{
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructWithDuplicatedConstraint::a1",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.a1, out)?;
            tezos_data_encoding::enc::field(
                "StructWithDuplicatedConstraint::a2",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.a2, out)?;
            Ok(())
        })(self, out)
    }
}
