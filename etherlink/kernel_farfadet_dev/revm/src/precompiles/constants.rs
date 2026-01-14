// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{Address, FixedBytes};

pub struct PredeployedContract {
    pub code: &'static [u8],
    pub code_hash: FixedBytes<32>,
}

pub(crate) const WITHDRAWAL_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/withdrawal.bin"),
    code_hash: FixedBytes::new([
        0x4a, 0xf6, 0x89, 0x51, 0x1e, 0xd7, 0xfc, 0x42, 0x02, 0xe5, 0x26, 0xcb, 0x25,
        0x6c, 0x1f, 0x91, 0xd3, 0xb4, 0x26, 0xdd, 0xd8, 0x17, 0x26, 0x04, 0xe4, 0x12,
        0xdb, 0x32, 0x6d, 0x74, 0x8e, 0xc8,
    ]),
};

pub(crate) const FA_BRIDGE_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/fa_bridge.bin"),
    code_hash: FixedBytes::new([
        0xc7, 0xd2, 0x32, 0xb2, 0x99, 0x02, 0x6a, 0xd3, 0xe8, 0xf9, 0x9d, 0xbd, 0x1a,
        0x4b, 0x6c, 0x56, 0x9e, 0xea, 0x90, 0x13, 0x27, 0x62, 0x78, 0x1f, 0xf5, 0x7b,
        0x74, 0xb4, 0x85, 0xbd, 0x4e, 0x45,
    ]),
};

pub(crate) const INTERNAL_FORWARDER_SOL_CONTRACT: PredeployedContract =
    PredeployedContract {
        code: include_bytes!("../../contracts/predeployed/internal_forwarder.bin"),
        code_hash: FixedBytes::new([
            0x20, 0x30, 0xb7, 0x2d, 0x11, 0xd8, 0x4d, 0x88, 0x76, 0xd5, 0x5e, 0x7a, 0xd0,
            0xff, 0x2e, 0xc0, 0x95, 0xaa, 0x8a, 0x61, 0x22, 0x72, 0x5b, 0xde, 0xdf, 0x96,
            0x25, 0x5b, 0xed, 0x29, 0xa9, 0x59,
        ]),
    };

pub const SYSTEM_SOL_ADDR: Address = Address::ZERO;

pub const WITHDRAWAL_SOL_ADDR: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
]));

pub const FA_BRIDGE_SOL_ADDR: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
]));

pub const FEED_DEPOSIT_ADDR: Address = Address(FixedBytes::new([
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xFE, 0xED,
]));

pub(crate) const SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS: Address =
    Address(FixedBytes::new([
        0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
    ]));

pub(crate) const TABLE_PRECOMPILE_ADDRESS: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x04,
]));

pub(crate) const GLOBAL_COUNTER_PRECOMPILE_ADDRESS: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x05,
]));

pub(crate) const CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS: Address =
    Address(FixedBytes::new([
        0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06,
    ]));

pub(crate) const RUNTIME_GATEWAY_PRECOMPILE_ADDRESS: Address =
    Address(FixedBytes::new([
        0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07,
    ]));

#[cfg(test)]
pub(crate) const PRECOMPILE_BURN_ADDRESS: Address = Address(FixedBytes::new([
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xde, 0xad,
]));

pub(crate) const CUSTOMS: [Address; 7] = [
    WITHDRAWAL_SOL_ADDR,
    FA_BRIDGE_SOL_ADDR,
    SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS,
    TABLE_PRECOMPILE_ADDRESS,
    GLOBAL_COUNTER_PRECOMPILE_ADDRESS,
    CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS,
    RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
];

// Rationale regarding the cost:
// A few decoding/encoding functions.
pub(crate) const SEND_OUTBOX_MESSAGE_BASE_COST: u64 = 500;

// Rationale regarding the cost:
// Covers the cost of 2 r/w access on cold keys.
// In particular, worst case for a cold read 2100 + the worst case for
// cold write 22100 (inserting a non zero value to a zero value).
pub(crate) const TICKET_TABLE_BASE_COST: u64 = 24_200;

// Rationale regarding the cost:
// Covers the cost of 2 r/w access on cold keys.
// In particular, worst case for a cold read 2100 + the worst case for
// cold write 22100 (inserting a non zero value to a zero value).
pub(crate) const GLOBAL_COUNTER_BASE_COST: u64 = 24_200;

// Rationale regarding the cost:
// Covers the cost of 2 r/w access on cold keys.
// In particular, worst case for a cold read 2100 + the worst case for
// cold write 22100 (inserting a non zero value to a zero value).
pub(crate) const UPGRADE_SEQUENCER_PRECOMPILE_BASE_COST: u64 = 24_200;

// Rationale regarding the cost:
// Consumed gas is ~81000 for both queue execute_without_proxy entrypoints
pub const FA_DEPOSIT_EXECUTION_COST: u64 = 100_000;

pub const FA_DEPOSIT_QUEUE_GAS_LIMIT: u64 = 0;

/// Overapproximation of the amount of ticks for parsing FA deposit.
/// Also includes hashing costs.
///
/// Obtained by running the `bench_fa_deposit` and examining both
/// `hashing_ticks` and `signature_verification_ticks` (parsing).
/// The final value is maximum total plus +50% reserve.
///
/// NOTE that we have a hard cap because of the maximum inbox message size limitation.
/// If it is lifted at some point in the future, we need to reflect that.
pub const TICKS_PER_FA_DEPOSIT_PARSING: u64 = 3_500_000;

pub(crate) const SEQUENCER_UPGRADE_DELAY: u64 = 60 * 60 * 24; // 24 hours

#[cfg(test)]
mod test {
    use super::{
        PredeployedContract, FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT,
        WITHDRAWAL_SOL_CONTRACT,
    };

    use crate::helpers::storage::bytes_hash;
    use revm::{primitives::Bytes, state::Bytecode};

    fn check_code_hash_validity(predeployed: &'static PredeployedContract) {
        let bytecode = Bytecode::new_legacy(Bytes::from_static(predeployed.code));
        let code_hash = bytes_hash(bytecode.original_byte_slice());
        // Keep this until the script is updated
        println!(
            "{:?}",
            code_hash
                .iter()
                .map(|e| format!("0x{e:02x}"))
                .collect::<Vec<String>>()
                .join(", ")
        );
        assert_eq!(code_hash, predeployed.code_hash)
    }

    #[test]
    fn check_withdrawal_sol_code_hash() {
        check_code_hash_validity(&WITHDRAWAL_SOL_CONTRACT)
    }

    #[test]
    fn check_fa_bridge_sol_code_hash() {
        check_code_hash_validity(&FA_BRIDGE_SOL_CONTRACT)
    }

    #[test]
    fn check_internal_forwarder_sol_code_hash() {
        check_code_hash_validity(&INTERNAL_FORWARDER_SOL_CONTRACT)
    }
}
