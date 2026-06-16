use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructBuiltins {
    a: i8,
    b: u8,
    c: i16,
    d: u16,
    e: i32,
    f: u32,
    g: i64,
    h: f64,
    i: bool,
}
impl tezos_data_encoding::encoding::HasEncoding for StructBuiltins {
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructBuiltins",
            <[_]>::into_vec(
                ::alloc::boxed::box_new([
                    tezos_data_encoding::encoding::Field::new(
                        "a",
                        tezos_data_encoding::encoding::Encoding::Int8,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "b",
                        tezos_data_encoding::encoding::Encoding::Uint8,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "c",
                        tezos_data_encoding::encoding::Encoding::Int16,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "d",
                        tezos_data_encoding::encoding::Encoding::Uint16,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "e",
                        tezos_data_encoding::encoding::Encoding::Int32,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "f",
                        tezos_data_encoding::encoding::Encoding::Uint32,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "g",
                        tezos_data_encoding::encoding::Encoding::Int64,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "h",
                        tezos_data_encoding::encoding::Encoding::Float,
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "i",
                        tezos_data_encoding::encoding::Encoding::Bool,
                    ),
                ]),
            ),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<'_a> tezos_data_encoding::nom::NomReader<'_a> for StructBuiltins {
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "StructBuiltins::a",
                    nom::number::complete::i8,
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::b",
                    nom::number::complete::u8,
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::c",
                    nom::number::complete::i16(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::d",
                    nom::number::complete::u16(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::e",
                    nom::number::complete::i32(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::f",
                    nom::number::complete::u32(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::g",
                    nom::number::complete::i64(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::h",
                    nom::number::complete::f64(nom::number::Endianness::Big),
                ),
                tezos_data_encoding::nom::field(
                    "StructBuiltins::i",
                    tezos_data_encoding::nom::boolean,
                ),
            )),
            |(a, b, c, d, e, f, g, h, i)| StructBuiltins {
                a,
                b,
                c,
                d,
                e,
                f,
                g,
                h,
                i,
            },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for StructBuiltins {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructBuiltins::a",
                tezos_data_encoding::enc::i8,
            )(&data.a, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::b",
                tezos_data_encoding::enc::u8,
            )(&data.b, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::c",
                tezos_data_encoding::enc::i16,
            )(&data.c, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::d",
                tezos_data_encoding::enc::u16,
            )(&data.d, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::e",
                tezos_data_encoding::enc::i32,
            )(&data.e, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::f",
                tezos_data_encoding::enc::u32,
            )(&data.f, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::g",
                tezos_data_encoding::enc::i64,
            )(&data.g, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::h",
                tezos_data_encoding::enc::f64,
            )(&data.h, out)?;
            tezos_data_encoding::enc::field(
                "StructBuiltins::i",
                tezos_data_encoding::enc::boolean,
            )(&data.i, out)?;
            Ok(())
        })(self, out)
    }
}
