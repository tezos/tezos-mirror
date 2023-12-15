// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::block_in_progress::BlockInProgress;
use crate::KernelUpgrade;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::rlp_helpers;

/// Data stored between kernel runs to resume the execution.

#[derive(Clone, Debug, PartialEq)]
pub struct RebootContext {
    pub kernel_upgrade: Option<KernelUpgrade>,
    pub bip: Option<BlockInProgress>,
}

impl Decodable for RebootContext {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let kernel_upgrade: Option<KernelUpgrade> =
            rlp_helpers::decode_option(&rlp_helpers::next(&mut it)?, "kernel_upgrade")?;
        let bip: Option<BlockInProgress> = rlp_helpers::decode_option(
            &rlp_helpers::next(&mut it)?,
            "block_in_progress",
        )?;
        Ok(Self {
            kernel_upgrade,
            bip,
        })
    }
}

impl Encodable for RebootContext {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        rlp_helpers::append_option(stream, &self.kernel_upgrade);
        rlp_helpers::append_option(stream, &self.bip);
    }
}

#[cfg(test)]
mod tests {

    use super::RebootContext;
    use crate::reboot_context::BlockInProgress;
    use crate::KernelUpgrade;
    use primitive_types::{H256, U256};
    use rlp::Encodable;
    use std::collections::VecDeque;
    use tezos_ethereum::rlp_helpers::FromRlpBytes;
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    fn encoding_roundtrip(v: RebootContext) {
        let bytes = v.rlp_bytes();
        let v2 = RebootContext::from_rlp_bytes(&bytes)
            .expect("Reboot Context should be decodable");
        assert_eq!(v, v2, "Roundtrip failed on {:?}", v)
    }

    fn dummy_bip(i: usize) -> BlockInProgress {
        BlockInProgress::new_with_ticks(
            U256::from(i),
            H256::zero(),
            U256::zero(),
            VecDeque::new(),
            0,
            Timestamp::from(0i64),
        )
    }

    fn dummy_reboot_context() -> RebootContext {
        let kernel_upgrade = Some(KernelUpgrade {
            preimage_hash: [3; PREIMAGE_HASH_SIZE],
        });
        let bip = Some(dummy_bip(2));
        RebootContext {
            kernel_upgrade,
            bip,
        }
    }

    #[test]
    fn roundtrip_rlp() {
        let v: RebootContext = dummy_reboot_context();
        encoding_roundtrip(v.clone());
        encoding_roundtrip(RebootContext {
            kernel_upgrade: None,
            ..v.clone()
        });
        encoding_roundtrip(RebootContext { bip: None, ..v });
        encoding_roundtrip(RebootContext {
            kernel_upgrade: None,
            bip: None,
        });
    }
}
