use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};
struct EmptyStruct {}
impl tezos_data_encoding::encoding::HasEncoding for EmptyStruct {
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        tezos_data_encoding::encoding::Encoding::Obj(
            "EmptyStruct",
            ::alloc::vec::Vec::new(),
        )
    }
}
#[allow(unused_parens)]
#[allow(clippy::unnecessary_cast)]
#[allow(clippy::redundant_closure_call)]
impl tezos_data_encoding::enc::BinWriter for EmptyStruct {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        (|data: &Self, out: &mut Vec<u8>| { Ok(()) })(self, out)
    }
}
