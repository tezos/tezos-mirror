// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{Address, FixedBytes};

pub struct PredeployedContract {
    pub code: &'static [u8],
    pub code_hash: FixedBytes<32>,
}

pub(crate) const XTZ_BRIDGE_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/xtz_bridge.bin"),
    code_hash: FixedBytes::new([
        0x0f, 0x7f, 0xf8, 0x19, 0x62, 0x44, 0xfb, 0xa3, 0xd3, 0xe3, 0xaf, 0xf7, 0x9a,
        0xe4, 0xb1, 0xc6, 0x6a, 0x4b, 0x6d, 0xd6, 0x20, 0x0f, 0xbf, 0x00, 0xf1, 0x7f,
        0x0b, 0x57, 0x44, 0xd5, 0x2a, 0xcf,
    ]),
};

pub(crate) const FA_BRIDGE_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/fa_bridge.bin"),
    code_hash: FixedBytes::new([
        0x97, 0xbc, 0x24, 0xdd, 0xa9, 0xc6, 0x51, 0xbd, 0xa3, 0xe0, 0xce, 0x31, 0x02,
        0xd0, 0xd0, 0xc2, 0x6a, 0x4a, 0xff, 0xea, 0x4f, 0x49, 0x13, 0x41, 0x33, 0xf4,
        0x7d, 0xcc, 0xd0, 0x14, 0xd6, 0x28,
    ]),
};

pub(crate) const INTERNAL_FORWARDER_SOL_CONTRACT: PredeployedContract =
    PredeployedContract {
        code: include_bytes!("../../contracts/predeployed/internal_forwarder.bin"),
        code_hash: FixedBytes::new([
            0x3d, 0x50, 0xa3, 0x68, 0xf2, 0xe3, 0x17, 0x13, 0xd6, 0x7b, 0x87, 0xf2, 0x24,
            0x43, 0xbd, 0x99, 0x7b, 0x3d, 0x0c, 0x98, 0x52, 0xad, 0xc4, 0x5a, 0xc1, 0xe1,
            0xc6, 0xee, 0x23, 0x95, 0x03, 0xfc,
        ]),
    };

pub const ALWAYS_REVERT_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/always_revert.bin"),
    code_hash: FixedBytes::new([
        0xdc, 0x27, 0x8f, 0x37, 0x0e, 0x1a, 0xd6, 0x65, 0x24, 0x70, 0x67, 0x16, 0x17,
        0x2b, 0x9a, 0x8a, 0x4c, 0xec, 0x0d, 0xb9, 0xff, 0xea, 0xc2, 0xe6, 0x12, 0x3b,
        0xbd, 0xc4, 0x8d, 0x74, 0x78, 0x1f,
    ]),
};

/// FA1.2 Wrapper contract deployed at a precompile address.
/// Provides an EVM-friendly interface for calling FA1.2 token contracts
/// on the Michelson runtime via the RuntimeGateway.
pub const FA12_WRAPPER_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/fa12_wrapper.bin"),
    code_hash: FixedBytes::new([
        0x04, 0xc9, 0xf7, 0xcb, 0x31, 0x9c, 0x77, 0x7b, 0x28, 0x6b, 0x99, 0x83, 0x9c,
        0x35, 0x7e, 0x32, 0x63, 0x23, 0x78, 0x1a, 0x1b, 0x85, 0x1b, 0x78, 0x04, 0x0e,
        0xbd, 0x71, 0x52, 0x40, 0xd4, 0xec,
    ]),
};

/// AliasForwarder contract deployed at a precompile address.
/// Alias addresses use EIP-7702 delegation to point to this contract.
pub const ALIAS_FORWARDER_SOL_CONTRACT: PredeployedContract = PredeployedContract {
    code: include_bytes!("../../contracts/predeployed/alias_forwarder.bin"),
    code_hash: FixedBytes::new([
        0xcb, 0x74, 0x28, 0x61, 0x61, 0xd1, 0x14, 0x05, 0xcd, 0x08, 0x55, 0xbd, 0x08,
        0xa7, 0x7c, 0x44, 0x73, 0xa4, 0x07, 0x84, 0x4f, 0xc3, 0x75, 0x31, 0x9d, 0xdd,
        0x74, 0x12, 0xb8, 0x41, 0xf2, 0xfe,
    ]),
};

pub const SYSTEM_SOL_ADDR: Address = Address::ZERO;

pub const XTZ_BRIDGE_SOL_ADDR: Address = Address(FixedBytes::new([
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

pub const RUNTIME_GATEWAY_PRECOMPILE_ADDRESS: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x07,
]));

/// Address where the AliasForwarder contract is deployed as a precompile.
/// Alias addresses use EIP-7702 delegation to delegate execution to this address.
pub const ALIAS_FORWARDER_PRECOMPILE_ADDRESS: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xff, 0xff, 0x08,
]));

pub const FA12_WRAPPER_SOL_ADDR: Address = Address(FixedBytes::new([
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xff, 0xff, 0x09,
]));

pub(crate) const VERIFY_TEZOS_SIGNATURE_PRECOMPILE_ADDRESS: Address =
    Address(FixedBytes::new([
        0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a,
    ]));

/// Dedicated caller address for TezosX internal transactions.
///
/// This address is used when the actual caller doesn't have an Ethereum-format
/// address (e.g., when initializing aliases for Tezos addresses). We use a specific
/// non-zero address because the zero address is reserved for simulation.
const TEZOSX_CALLER_ADDRESS_BYTES: [u8; 20] = [
    0x7e, 0x20, 0x58, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
];

pub const TEZOSX_CALLER_ADDRESS: Address = Address::new(TEZOSX_CALLER_ADDRESS_BYTES);

/// Same address as [`TEZOSX_CALLER_ADDRESS`] but as `primitive_types::H160`,
/// for use in contexts that don't depend on alloy types.
pub const TEZOSX_CALLER_H160: primitive_types::H160 =
    primitive_types::H160(TEZOSX_CALLER_ADDRESS_BYTES);

#[cfg(test)]
pub(crate) const PRECOMPILE_BURN_ADDRESS: Address = Address(FixedBytes::new([
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xde, 0xad,
]));

pub(crate) const CUSTOMS: [Address; 10] = [
    XTZ_BRIDGE_SOL_ADDR,
    FA_BRIDGE_SOL_ADDR,
    SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS,
    TABLE_PRECOMPILE_ADDRESS,
    GLOBAL_COUNTER_PRECOMPILE_ADDRESS,
    CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS,
    RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
    ALIAS_FORWARDER_PRECOMPILE_ADDRESS,
    FA12_WRAPPER_SOL_ADDR,
    VERIFY_TEZOS_SIGNATURE_PRECOMPILE_ADDRESS,
];

// Rationale regarding the cost:
// Worst-case across Ed25519, secp256k1, and P-256. Dominated by the
// P-256 verification (RIP-7212 native cost is 6 900 scaled 2-3x for
// a non-native impl). The remainder covers ABI decode, pubkey and
// signature parsing, blake2, low-S malleability check.
//
// TODO: benchmark to confirm
// TODO: when integrating BLS (tz4), this value should be significantly increased
pub(crate) const VERIFY_TEZOS_SIGNATURE_BASE_COST: u64 = 20_000;

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

// Gateway base cost: covers ABI decoding, HTTP request construction,
// header injection (7 X-Tezos-* headers), gas conversion arithmetic,
// response parsing, and CracSent event emission (LOG4).
// Derived from Ethereum gas schedule: G_log=375, G_logtopic=375,
// G_logdata=8/byte, G_copy=3/word, G_memory=3/word.
// See RFC: Gas model for gateways and aliases.
pub(crate) const RUNTIME_GATEWAY_BASE_COST: u64 = 5_000;

// Per 32-byte word surcharge applied on top of the flat base cost for
// every gateway entrypoint, proportional to calldata + outgoing body +
// incoming response body. Matches `G_copy` (3/word), the same rate
// REVM uses for CALLDATACOPY/RETURNDATACOPY memory expansion — but
// charged here *inside* the precompile, since REVM does not
// automatically meter per-byte work performed by custom precompiles
// (ABI decode, `to_vec()` clones, HeaderMap inserts).
pub(crate) const RUNTIME_GATEWAY_PER_WORD_COST: u64 = 3;

// Alias lookup: charged upfront before each alias resolution (durable
// storage read). Equivalent to a cold SLOAD (EIP-2929).
pub(crate) const ALIAS_LOOKUP_COST: u64 = 2_100;

// Surcharge when msg.value > 0 (precompile balance burn after cross-runtime
// value transfer). Equivalent to SSTORE non-zero to zero (EIP-2929 + YP).
pub(crate) const VALUE_TRANSFER_SURCHARGE: u64 = 5_000;

// Per user-supplied header validation cost for the generic call()
// function. Covers the byte-level prefix check against forbidden
// X-Tezos-* headers *and* the subsequent insertion into the HTTP
// `HeaderMap` (hash of the header name + bucket insert + potential
// rehash).
pub(crate) const HEADER_VALIDATION_PER_HEADER: u64 = 100;

// Rationale regarding the cost:
// Consumed gas is ~81000 for both queue execute_without_proxy entrypoints
pub const FA_DEPOSIT_EXECUTION_COST: u64 = 100_000;

pub const FA_DEPOSIT_QUEUE_GAS_LIMIT: u64 = 0;

// Rationale regarding the cost:
// The execution cost is not meant to price the deposit operation economically.
// Deposits are intended to be free for users.
// This fixed cost exists solely to account for the EVM gas consumed by the
// `handle_xtz_deposit` entrypoint and to deliberately limit any remaining gas,
// preventing its use for unintended or malicious execution paths.
// For this reason, the value is kept "small" but non-zero.
// The exact max gas consuption is 164_261, which we round up as this does not
// introduce any security concerns.
pub const XTZ_DEPOSIT_EXECUTION_COST: u64 = 175_000;

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
        PredeployedContract, ALIAS_FORWARDER_SOL_CONTRACT, ALWAYS_REVERT_SOL_CONTRACT,
        FA12_WRAPPER_SOL_CONTRACT, FA_BRIDGE_SOL_CONTRACT,
        INTERNAL_FORWARDER_SOL_CONTRACT, XTZ_BRIDGE_SOL_CONTRACT,
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
    fn check_xtz_bridge_sol_code_hash() {
        check_code_hash_validity(&XTZ_BRIDGE_SOL_CONTRACT)
    }

    #[test]
    fn check_fa_bridge_sol_code_hash() {
        check_code_hash_validity(&FA_BRIDGE_SOL_CONTRACT)
    }

    #[test]
    fn check_internal_forwarder_sol_code_hash() {
        check_code_hash_validity(&INTERNAL_FORWARDER_SOL_CONTRACT)
    }

    #[test]
    fn check_always_revert_sol_code_hash() {
        check_code_hash_validity(&ALWAYS_REVERT_SOL_CONTRACT)
    }

    #[test]
    fn check_alias_forwarder_sol_code_hash() {
        check_code_hash_validity(&ALIAS_FORWARDER_SOL_CONTRACT)
    }

    #[test]
    fn check_fa12_wrapper_sol_code_hash() {
        check_code_hash_validity(&FA12_WRAPPER_SOL_CONTRACT)
    }
}
