use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct StructComposite<A: Clone> {
    a: A,
    #[encoding(dynamic)]
    b: A,
    #[encoding(dynamic, list)]
    c: Vec<A>,
    #[encoding(dynamic, bytes)]
    d: Vec<u8>,
    #[encoding(short_dynamic, bytes)]
    e: Vec<u8>,
    #[encoding(dynamic, list, dynamic, list)]
    f: Vec<Vec<A>>,
    #[encoding(bounded = "10")]
    g: A,
    #[encoding(list = "10")]
    h: Vec<A>,
    #[encoding(dynamic = "10")]
    i: A,
    #[encoding(skip)]
    j: A,
    #[encoding(string = "10")]
    k: String,
    #[encoding(reserve = "10")]
    l: A,
}
impl<A: Clone> tezos_data_encoding::encoding::HasEncoding for StructComposite<A>
where
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
    A: tezos_data_encoding::encoding::HasEncoding,
{
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "StructComposite",
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
                        tezos_data_encoding::encoding::Encoding::Dynamic(
                            Box::new(
                                #[allow(clippy::redundant_clone)]
                                <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                    .clone(),
                            ),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "c",
                        tezos_data_encoding::encoding::Encoding::Dynamic(
                            Box::new(
                                tezos_data_encoding::encoding::Encoding::List(
                                    Box::new(
                                        #[allow(clippy::redundant_clone)]
                                        <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                            .clone(),
                                    ),
                                ),
                            ),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "d",
                        tezos_data_encoding::encoding::Encoding::Dynamic(
                            Box::new(tezos_data_encoding::encoding::Encoding::Bytes),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "e",
                        tezos_data_encoding::encoding::Encoding::ShortDynamic(
                            Box::new(tezos_data_encoding::encoding::Encoding::Bytes),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "f",
                        tezos_data_encoding::encoding::Encoding::Dynamic(
                            Box::new(
                                tezos_data_encoding::encoding::Encoding::List(
                                    Box::new(
                                        tezos_data_encoding::encoding::Encoding::Dynamic(
                                            Box::new(
                                                tezos_data_encoding::encoding::Encoding::List(
                                                    Box::new(
                                                        #[allow(clippy::redundant_clone)]
                                                        <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                                            .clone(),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "g",
                        tezos_data_encoding::encoding::Encoding::Bounded(
                            10,
                            Box::new(
                                #[allow(clippy::redundant_clone)]
                                <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                    .clone(),
                            ),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "h",
                        tezos_data_encoding::encoding::Encoding::BoundedList(
                            10,
                            Box::new(
                                #[allow(clippy::redundant_clone)]
                                <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                    .clone(),
                            ),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "i",
                        tezos_data_encoding::encoding::Encoding::BoundedDynamic(
                            10,
                            Box::new(
                                #[allow(clippy::redundant_clone)]
                                <A as tezos_data_encoding::encoding::HasEncoding>::encoding()
                                    .clone(),
                            ),
                        ),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "k",
                        tezos_data_encoding::encoding::Encoding::BoundedString(10),
                    ),
                    tezos_data_encoding::encoding::Field::new(
                        "l",
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
impl<'_a, A: Clone> tezos_data_encoding::nom::NomReader<'_a> for StructComposite<A>
where
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
    A: for<'a> tezos_data_encoding::nom::NomReader<'a>,
{
    fn nom_read(bytes: &'_a [u8]) -> tezos_data_encoding::nom::NomResult<'_a, Self> {
        nom::combinator::map(
            nom::sequence::tuple((
                tezos_data_encoding::nom::field(
                    "StructComposite::a",
                    <A as tezos_data_encoding::nom::NomReader>::nom_read,
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::b",
                    tezos_data_encoding::nom::dynamic(
                        <A as tezos_data_encoding::nom::NomReader>::nom_read,
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::c",
                    tezos_data_encoding::nom::dynamic(
                        tezos_data_encoding::nom::list(
                            <A as tezos_data_encoding::nom::NomReader>::nom_read,
                        ),
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::d",
                    tezos_data_encoding::nom::dynamic(tezos_data_encoding::nom::bytes),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::e",
                    tezos_data_encoding::nom::short_dynamic(
                        tezos_data_encoding::nom::bytes,
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::f",
                    tezos_data_encoding::nom::dynamic(
                        tezos_data_encoding::nom::list(
                            tezos_data_encoding::nom::dynamic(
                                tezos_data_encoding::nom::list(
                                    <A as tezos_data_encoding::nom::NomReader>::nom_read,
                                ),
                            ),
                        ),
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::g",
                    tezos_data_encoding::nom::bounded(
                        10,
                        <A as tezos_data_encoding::nom::NomReader>::nom_read,
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::h",
                    tezos_data_encoding::nom::bounded_list(
                        10,
                        <A as tezos_data_encoding::nom::NomReader>::nom_read,
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::i",
                    tezos_data_encoding::nom::bounded_dynamic(
                        10,
                        <A as tezos_data_encoding::nom::NomReader>::nom_read,
                    ),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::j",
                    |input| Ok((input, Default::default())),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::k",
                    tezos_data_encoding::nom::bounded_string(10),
                ),
                tezos_data_encoding::nom::field(
                    "StructComposite::l",
                    tezos_data_encoding::nom::reserve(
                        10,
                        <A as tezos_data_encoding::nom::NomReader>::nom_read,
                    ),
                ),
            )),
            |(a, b, c, d, e, f, g, h, i, j, k, l)| StructComposite {
                a,
                b,
                c,
                d,
                e,
                f,
                g,
                h,
                i,
                j,
                k,
                l,
            },
        )(bytes)
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl<A: Clone> tezos_data_encoding::enc::BinWriter for StructComposite<A>
where
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
    A: tezos_data_encoding::enc::BinWriter,
{
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| {
            tezos_data_encoding::enc::field(
                "StructComposite::a",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.a, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::b",
                tezos_data_encoding::enc::dynamic(
                    <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                ),
            )(&data.b, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::c",
                tezos_data_encoding::enc::dynamic(
                    tezos_data_encoding::enc::list(
                        <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                    ),
                ),
            )(&data.c, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::d",
                tezos_data_encoding::enc::dynamic(tezos_data_encoding::enc::bytes),
            )(&data.d, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::e",
                tezos_data_encoding::enc::short_dynamic(tezos_data_encoding::enc::bytes),
            )(&data.e, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::f",
                tezos_data_encoding::enc::dynamic(
                    tezos_data_encoding::enc::list(
                        tezos_data_encoding::enc::dynamic(
                            tezos_data_encoding::enc::list(
                                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                            ),
                        ),
                    ),
                ),
            )(&data.f, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::g",
                tezos_data_encoding::enc::bounded(
                    10,
                    <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                ),
            )(&data.g, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::h",
                tezos_data_encoding::enc::bounded_list(
                    10,
                    <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                ),
            )(&data.h, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::i",
                tezos_data_encoding::enc::bounded_dynamic(
                    10,
                    <A as tezos_data_encoding::enc::BinWriter>::bin_write,
                ),
            )(&data.i, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::k",
                tezos_data_encoding::enc::bounded_string(10),
            )(&data.k, out)?;
            tezos_data_encoding::enc::field(
                "StructComposite::l",
                <A as tezos_data_encoding::enc::BinWriter>::bin_write,
            )(&data.l, out)?;
            Ok(())
        })(self, out)
    }
}
