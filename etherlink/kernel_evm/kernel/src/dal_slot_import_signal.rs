// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::hash::UnknownSignature;
use tezos_ethereum::rlp_helpers::{self, append_u32_le, decode_field_u32_le};

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotIndicesList(pub Vec<u8>);

impl Encodable for DalSlotIndicesList {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(self.0.len());
        for slot in &self.0 {
            stream.append(slot);
        }
    }
}

impl Decodable for DalSlotIndicesList {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_is_list(decoder)?;
        let slot_indices: Vec<u8> = rlp_helpers::decode_list(decoder, "slot_indices")?;
        Ok(DalSlotIndicesList(slot_indices))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotIndicesOfLevel {
    pub published_level: u32,
    pub slot_indices: DalSlotIndicesList,
}

impl Encodable for DalSlotIndicesOfLevel {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        append_u32_le(stream, &self.published_level);
        stream.append(&self.slot_indices);
    }
}

impl Decodable for DalSlotIndicesOfLevel {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 2)?;
        let mut it = decoder.iter();
        let published_level: u32 =
            decode_field_u32_le(&rlp_helpers::next(&mut it)?, "published_level")?;
        let slot_indices =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "slot_indices")?;
        Ok(DalSlotIndicesOfLevel {
            published_level,
            slot_indices,
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnsignedDalSlotSignals(pub Vec<DalSlotIndicesOfLevel>);

impl Encodable for UnsignedDalSlotSignals {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(self.0.len());
        for slots_with_level in &self.0 {
            stream.append(slots_with_level);
        }
    }
}

impl Decodable for UnsignedDalSlotSignals {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_is_list(decoder)?;
        let levels_with_slots: Vec<DalSlotIndicesOfLevel> =
            rlp_helpers::decode_list(decoder, "unsigned_dal_slots_signals")?;
        Ok(UnsignedDalSlotSignals(levels_with_slots))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotImportSignals {
    pub signals: UnsignedDalSlotSignals,
    pub signature: UnknownSignature,
}

impl Encodable for DalSlotImportSignals {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append(&self.signals);
        stream.append(&self.signature.as_ref());
    }
}

impl Decodable for DalSlotImportSignals {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 2)?;
        let mut it = decoder.iter();
        let signals =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "unsigned_signals")?;
        let signature_bytes: Vec<u8> =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "signature")?;
        let signature = UnknownSignature::try_from(signature_bytes.as_slice())
            .map_err(|_| DecoderError::Custom("Invalid signature encoding"))?;
        Ok(DalSlotImportSignals { signals, signature })
    }
}

#[cfg(test)]
mod tests {
    use super::{
        DalSlotImportSignals, DalSlotIndicesList, DalSlotIndicesOfLevel,
        UnsignedDalSlotSignals,
    };
    use rlp::{Decodable, DecoderError, Encodable};
    use tezos_crypto_rs::hash::UnknownSignature;
    use tezos_ethereum::rlp_helpers::FromRlpBytes;

    #[derive(PartialEq, Debug, Clone)]
    pub enum RlpTree {
        Val(Vec<u8>),
        List(Vec<RlpTree>),
    }

    impl Decodable for RlpTree {
        fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
            if decoder.is_list() {
                let l: Vec<RlpTree> = decoder.as_list()?;
                Ok(RlpTree::List(l))
            } else {
                let s: Vec<u8> = decoder.as_val()?;
                Ok(RlpTree::Val(s))
            }
        }
    }

    impl Encodable for RlpTree {
        fn rlp_append(&self, stream: &mut rlp::RlpStream) {
            match self {
                RlpTree::List(trees) => {
                    stream.begin_list(trees.len());
                    for tree in trees {
                        stream.append(tree);
                    }
                }
                RlpTree::Val(bytes) => {
                    stream.append(bytes);
                }
            };
        }
    }

    fn index_expected(v: u8, tree: RlpTree) {
        let encoded = v.rlp_bytes();
        let encoded_as_tree: RlpTree =
            FromRlpBytes::from_rlp_bytes(&encoded).expect("should decode as tree");
        assert_eq!(
            encoded_as_tree, tree,
            "Encoding gave unexpected result, source: {:?}, expected: {:?}, actual: {:?}",
            v, tree, encoded_as_tree
        );

        let v2: u8 =
            FromRlpBytes::from_rlp_bytes(&encoded).expect("Index should be decodable");
        assert_eq!(v, v2);
    }

    fn indices_expected(v: Vec<u8>, tree: RlpTree) {
        let v = DalSlotIndicesList(v);
        let encoded = v.rlp_bytes();
        let encoded_as_tree: RlpTree =
            FromRlpBytes::from_rlp_bytes(&encoded).expect("should decode as tree");
        assert_eq!(
            encoded_as_tree, tree,
            "Encoding gave unexpected result, source: {:?}, expected: {:?}, actual: {:?}",
            v, tree, encoded_as_tree
        );

        let v2: DalSlotIndicesList =
            FromRlpBytes::from_rlp_bytes(&encoded).expect("Indices should be decodable");
        assert_eq!(v, v2);
    }

    fn indices_roundtrip(v: Vec<u8>) {
        let v = DalSlotIndicesList(v);
        let bytes = v.rlp_bytes();
        let v2: DalSlotIndicesList =
            FromRlpBytes::from_rlp_bytes(&bytes).expect("Indices should be decodable");
        assert_eq!(v, v2, "Roundtrip failed for indices: {:?}", v);
    }

    fn indices_of_level_roundtrip(published_level: u32, slot_indices: Vec<u8>) {
        let slot_indices = DalSlotIndicesList(slot_indices);
        let v = DalSlotIndicesOfLevel {
            published_level,
            slot_indices,
        };
        let bytes = v.rlp_bytes();
        let v2: DalSlotIndicesOfLevel = FromRlpBytes::from_rlp_bytes(&bytes)
            .expect("Slot indices of level should be decodable");
        assert_eq!(v, v2, "Roundtrip failed for slot indices of level: {:?}", v);
    }

    fn unsigned_signals_roundtrip(v: UnsignedDalSlotSignals) {
        let bytes = v.rlp_bytes();
        let v2: UnsignedDalSlotSignals = FromRlpBytes::from_rlp_bytes(&bytes)
            .expect("Unsigned signals should be decodable");
        assert_eq!(v, v2, "Roundtrip failed for unsigned signals: {:?}", v);

        let signature = UnknownSignature::from_base58_check(
            "sigdGBG68q2vskMuac4AzyNb1xCJTfuU8MiMbQtmZLUCYydYrtTd5Lessn1EFLTDJzjXoYxRasZxXbx6tHnirbEJtikcMHt3"
        ).expect("signature decoding should work");

        let v3 = DalSlotImportSignals {
            signals: v,
            signature,
        };
        let bytes = v3.rlp_bytes();
        let v4: DalSlotImportSignals = FromRlpBytes::from_rlp_bytes(&bytes)
            .expect("Signed signals should be decodable");
        assert_eq!(v3, v4, "Roundtrip failed for signed signals: {:?}", v3)
    }

    #[test]
    fn indices_roundtrip_tests() {
        indices_roundtrip(vec![]);
        indices_roundtrip(vec![0]);
        indices_roundtrip(vec![1]);
        indices_roundtrip(vec![0, 1]);
    }

    #[test]
    fn index_expected_zero() {
        index_expected(0, RlpTree::Val(vec![]));
    }

    #[test]
    fn index_expected_one() {
        index_expected(1, RlpTree::Val(vec![1]));
    }

    #[test]
    fn index_expected_128() {
        index_expected(128, RlpTree::Val(vec![128]));
    }

    #[test]
    fn indices_expected_empty() {
        indices_expected(vec![], RlpTree::List(vec![]));
    }

    #[test]
    fn indices_expected_zero() {
        indices_expected(vec![0], RlpTree::List(vec![RlpTree::Val(vec![])]));
    }

    #[test]
    fn indices_expected_one() {
        indices_expected(vec![1], RlpTree::List(vec![RlpTree::Val(vec![1])]));
    }

    #[test]
    fn indices_expected_zero_zero() {
        indices_expected(
            vec![0, 0],
            RlpTree::List(vec![RlpTree::Val(vec![]), RlpTree::Val(vec![])]),
        );
    }

    #[test]
    fn indices_expected_zero_one() {
        indices_expected(
            vec![0, 1],
            RlpTree::List(vec![RlpTree::Val(vec![]), RlpTree::Val(vec![1])]),
        );
    }

    #[test]
    fn indices_expected_one_one() {
        indices_expected(
            vec![1, 1],
            RlpTree::List(vec![RlpTree::Val(vec![1]), RlpTree::Val(vec![1])]),
        );
    }

    #[test]
    fn indices_expected_one_one_one_one() {
        indices_expected(
            vec![1, 1, 1, 1],
            RlpTree::List(vec![
                RlpTree::Val(vec![1]),
                RlpTree::Val(vec![1]),
                RlpTree::Val(vec![1]),
                RlpTree::Val(vec![1]),
            ]),
        );
    }

    #[test]
    fn indices_expected_zero_zero_zero_zero() {
        indices_expected(
            vec![0, 0, 0, 0],
            RlpTree::List(vec![
                RlpTree::Val(vec![]),
                RlpTree::Val(vec![]),
                RlpTree::Val(vec![]),
                RlpTree::Val(vec![]),
            ]),
        );
    }

    #[test]
    fn indices_of_level_roundtrip_tests() {
        indices_of_level_roundtrip(0, vec![]);
        indices_of_level_roundtrip(0, vec![0]);
        indices_of_level_roundtrip(0, vec![1]);
        indices_of_level_roundtrip(0, vec![0, 1]);
        indices_of_level_roundtrip(100, vec![]);
        indices_of_level_roundtrip(100, vec![0]);
        indices_of_level_roundtrip(100, vec![1]);
        indices_of_level_roundtrip(100, vec![0, 1]);
    }

    #[test]
    fn roundtrip_empty() {
        let v = UnsignedDalSlotSignals(vec![]);
        unsigned_signals_roundtrip(v);
    }

    #[test]
    fn roundtrip_single_empty() {
        let indices_of_level = DalSlotIndicesOfLevel {
            published_level: 0,
            slot_indices: DalSlotIndicesList(vec![]),
        };

        let v = UnsignedDalSlotSignals(vec![indices_of_level]);
        unsigned_signals_roundtrip(v);
    }

    #[test]
    fn roundtrip_single_zero() {
        let indices_of_level = DalSlotIndicesOfLevel {
            published_level: 0,
            slot_indices: DalSlotIndicesList(vec![0]),
        };

        let v = UnsignedDalSlotSignals(vec![indices_of_level]);
        unsigned_signals_roundtrip(v);
    }

    #[test]
    fn roundtrip_single_one() {
        let indices_of_level = DalSlotIndicesOfLevel {
            published_level: 0,
            slot_indices: DalSlotIndicesList(vec![1]),
        };

        let v = UnsignedDalSlotSignals(vec![indices_of_level]);
        unsigned_signals_roundtrip(v);
    }

    #[test]
    fn roundtrip_single_zero_one() {
        let indices_of_level = DalSlotIndicesOfLevel {
            published_level: 0,
            slot_indices: DalSlotIndicesList(vec![0, 1]),
        };

        let v = UnsignedDalSlotSignals(vec![indices_of_level]);
        unsigned_signals_roundtrip(v);
    }
}
