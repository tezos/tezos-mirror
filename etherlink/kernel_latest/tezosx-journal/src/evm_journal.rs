// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;
use revm::{
    context::{transaction::AccessList, JournalInner},
    primitives::{Address, U256},
    JournalEntry,
};
use tezos_ethereum::block::BlockConstants;
use tezosx_types::RuntimeId;

use crate::LayeredState;

/// Header data from the first incoming CRAC, used to populate the
/// fields of the fake EVM transaction built at the end of a
/// foreign-runtime transaction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CracTransactionInfo {
    /// EVM address (alias) of the top-level transaction originator (from X-Tezos-Source).
    pub source: Address,
    /// EVM address (alias) of the immediate caller (from X-Tezos-Sender).
    pub sender: Address,
    /// Gas limit forwarded to the call (from X-Tezos-Gas-Limit).
    pub gas_limit: U256,
    /// Value attached to the call (from X-Tezos-Amount), in wei.
    pub amount: U256,
}

/// Identity of the top-level transaction originator (`tx.origin`),
/// carried across re-entrant runtime frames. `native_address` is in
/// the originating runtime's own encoding (lowercase 0x hex for
/// Ethereum, b58check PKH for Tezos); `evm_alias` is the
/// Etherlink-side `Address` used by `tezosx_resolve_source_alias`,
/// cached at capture time so consumers do not pay a `keccak`
/// round-trip on every gateway call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OriginalSource {
    runtime: RuntimeId,
    native_address: String,
    evm_alias: Address,
}

impl OriginalSource {
    pub fn new(runtime: RuntimeId, native_address: String, evm_alias: Address) -> Self {
        Self {
            runtime,
            native_address,
            evm_alias,
        }
    }

    pub fn runtime(&self) -> RuntimeId {
        self.runtime
    }

    pub fn native_address(&self) -> &str {
        &self.native_address
    }

    pub fn evm_alias(&self) -> Address {
        self.evm_alias
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct EvmJournal {
    pub layered_state: LayeredState,
    pub inner: JournalInner<JournalEntry>,
    access_list: Option<AccessList>,
    /// Header info from the first incoming CRAC.
    crac_tx_info: Option<CracTransactionInfo>,
    /// Original source (E_0 / `tx.origin`) of the current top-level
    /// transaction, captured on the first outgoing gateway call.
    /// Lives on `EvmJournal` so it survives re-entrant EVM frames
    /// spawned by Michelson `call_evm` — the inner frame's
    /// `tx().caller()` would be the alias of the Michelson contract,
    /// not the original source.
    original_source: Option<OriginalSource>,
    crac_chain_depth: u32,
    revm_call_depth: Option<u32>,
    /// Originator of the inbound CRAC being serviced — the
    /// `X-Tezos-Source` alias parsed into an EVM `Address`. When `Some`,
    /// the kernel's custom `ORIGIN` opcode pushes this address instead
    /// of `TxEnv.caller`, so `tx.origin` carries the real outer-tx
    /// originator across an `EVM <-> Michelson` boundary while
    /// `TxEnv.caller` (and thus `msg.sender`, the nonce bump, the value
    /// deduction, etc.) keep their standard REVM meaning — the
    /// immediate caller's alias (L2-1363 / L2-1441).
    cross_runtime_originator: Option<Address>,
    /// The originating runtime's L2 block environment, supplied at journal
    /// creation (a native EVM transaction's block, or the EVM-runtime
    /// block constants of a Michelson operation). When servicing an
    /// inbound CRAC, `create_block_constants` inherits the block-level
    /// observables from it — `BASEFEE`, `GASLIMIT` (block gas limit),
    /// `BLOBBASEFEE`, `PREVRANDAO`, coinbase, number, timestamp — so EVM
    /// bytecode reached through the gateway sees the same block as the
    /// native path instead of zero/placeholder values. Block constants are
    /// block-invariant, so the value set at the top is correct for every
    /// re-entrant frame (L2-1417).
    outer_block: BlockConstants,
}

impl EvmJournal {
    /// Construct an EVM journal seeded with the originating block
    /// environment. The block is supplied at creation time — forwarded by
    /// [`crate::TezosXJournal::new`] — rather than poked in afterwards, so
    /// the cross-runtime journal owns the single point that distributes
    /// block context to each runtime's sub-journal.
    pub fn new(outer_block: BlockConstants) -> Self {
        Self {
            layered_state: LayeredState::new(),
            inner: JournalInner::new(),
            access_list: None,
            crac_tx_info: None,
            original_source: None,
            crac_chain_depth: 0,
            revm_call_depth: None,
            cross_runtime_originator: None,
            outer_block,
        }
    }
}

impl EvmJournal {
    /// Discard all accumulated EVM state.
    pub fn clear(&mut self) {
        let _ = self.inner.finalize();
        let _ = self.layered_state.finalize();
        self.crac_tx_info = None;
        self.access_list = None;
        self.original_source = None;
        self.crac_chain_depth = 0;
        self.revm_call_depth = None;
        self.cross_runtime_originator = None;
        // `outer_block` is intentionally NOT reset: it is
        // top-level-transaction context, set once at creation and
        // invariant for the whole operation. `clear()` runs on a
        // backtracked Michelson operation to drop accumulated EVM state,
        // but a later CRAC in the same operation must still observe the
        // same block — wiping it here would re-zero those observables.
    }

    /// See the field doc on [`Self::cross_runtime_originator`].
    pub fn cross_runtime_originator(&self) -> Option<Address> {
        self.cross_runtime_originator
    }

    /// See the field doc on [`Self::cross_runtime_originator`].
    pub fn set_cross_runtime_originator(&mut self, originator: Option<Address>) {
        self.cross_runtime_originator = originator;
    }

    /// See the field doc on [`Self::outer_block`].
    pub fn outer_block(&self) -> &BlockConstants {
        &self.outer_block
    }

    pub fn crac_chain_depth(&self) -> u32 {
        self.crac_chain_depth
    }

    pub fn set_crac_chain_depth(&mut self, depth: u32) {
        self.crac_chain_depth = depth;
    }

    pub fn revm_call_depth(&self) -> Option<u32> {
        self.revm_call_depth
    }

    pub fn set_revm_call_depth(&mut self, depth: Option<u32>) {
        self.revm_call_depth = depth;
    }

    pub fn original_source(&self) -> Option<&OriginalSource> {
        self.original_source.as_ref()
    }

    /// First call captures; subsequent calls are no-ops so re-entrant
    /// frames cannot overwrite the original identity.
    pub fn set_original_source(&mut self, source: OriginalSource) {
        if self.original_source.is_none() {
            self.original_source = Some(source);
        }
    }

    pub fn set_access_list(
        &mut self,
        input_list: AccessList,
    ) -> Result<(), anyhow::Error> {
        if self.access_list.is_none() {
            self.access_list = Some(input_list);
            Ok(())
        } else {
            Err(anyhow!("Access list should only be set once per context"))
        }
    }

    pub fn get_access_list_copy(&self) -> AccessList {
        self.access_list.clone().unwrap_or_default()
    }

    /// Set CRAC transaction info from the first incoming CRAC headers.
    pub fn set_crac_tx_info(
        &mut self,
        info: CracTransactionInfo,
    ) -> Result<(), anyhow::Error> {
        if self.crac_tx_info.is_some() {
            return Err(anyhow!(
                "CRAC transaction info should only be set once per context"
            ));
        }
        self.crac_tx_info = Some(info);
        Ok(())
    }

    /// Take accumulated CRAC data (logs + tx info).
    /// Returns `None` if no incoming CRAC happened.
    pub fn take_crac_data(&mut self) -> Option<CracTransactionInfo> {
        let info = self.crac_tx_info.take()?;
        Some(info)
    }

    /// Whether an incoming CRAC has been received.
    pub fn has_crac_data(&self) -> bool {
        self.crac_tx_info.is_some()
    }
}

impl Default for EvmJournal {
    fn default() -> Self {
        Self::new(BlockConstants::dummy())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use revm::primitives::Address;

    #[test]
    fn test_set_crac_tx_info_errors_on_duplicate() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        let info1 = CracTransactionInfo {
            source: Address::from([0x11; 20]),
            sender: Address::from([0x22; 20]),
            gas_limit: U256::from(1000),
            amount: U256::from(42),
        };
        let info2 = CracTransactionInfo {
            source: Address::from([0x33; 20]),
            sender: Address::from([0x44; 20]),
            gas_limit: U256::from(2000),
            amount: U256::from(99),
        };
        journal.set_crac_tx_info(info1).unwrap();
        let err = journal.set_crac_tx_info(info2);
        assert!(err.is_err());
    }

    #[test]
    fn test_take_crac_data_returns_none_when_no_crac() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        assert!(journal.take_crac_data().is_none());
    }

    #[test]
    fn test_take_crac_data_returns_none_without_tx_info() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        assert!(journal.take_crac_data().is_none());
    }

    #[test]
    fn test_take_crac_data_with_info_but_no_logs() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        journal
            .set_crac_tx_info(CracTransactionInfo {
                source: Address::from([0x11; 20]),
                sender: Address::from([0x22; 20]),
                gas_limit: U256::from(1000),
                amount: U256::ZERO,
            })
            .unwrap();
        let _ = journal.take_crac_data().unwrap();
        assert!(!journal.has_crac_data());
    }

    #[test]
    fn test_take_crac_data_consumes() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        journal
            .set_crac_tx_info(CracTransactionInfo {
                source: Address::from([0x11; 20]),
                sender: Address::from([0x22; 20]),
                gas_limit: U256::from(1000),
                amount: U256::ZERO,
            })
            .unwrap();

        let _ = journal.take_crac_data().unwrap();
        assert!(journal.take_crac_data().is_none());
        assert!(!journal.has_crac_data());
    }

    #[test]
    fn test_original_source_starts_unset() {
        let journal = EvmJournal::new(BlockConstants::dummy());
        assert_eq!(journal.original_source(), None);
    }

    #[test]
    fn test_original_source_first_set_wins() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        let eoa = OriginalSource::new(
            RuntimeId::Ethereum,
            "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee".to_string(),
            Address::from([0xEE; 20]),
        );
        let intermediate = OriginalSource::new(
            RuntimeId::Ethereum,
            "0xcccccccccccccccccccccccccccccccccccccccc".to_string(),
            Address::from([0xCC; 20]),
        );

        journal.set_original_source(eoa.clone());
        assert_eq!(journal.original_source(), Some(&eoa));

        journal.set_original_source(intermediate);
        assert_eq!(journal.original_source(), Some(&eoa));
    }

    #[test]
    fn test_original_source_preserves_tezos_pkh() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        let tz_origin = OriginalSource::new(
            RuntimeId::Tezos,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string(),
            Address::from([0xAB; 20]),
        );
        journal.set_original_source(tz_origin.clone());
        let stored = journal.original_source().unwrap();
        assert_eq!(stored, &tz_origin);
        assert_eq!(stored.runtime(), RuntimeId::Tezos);
        assert_eq!(
            stored.native_address(),
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
        );
        assert_eq!(stored.evm_alias(), Address::from([0xAB; 20]));
    }

    #[test]
    fn test_clear_resets_original_source() {
        let mut journal = EvmJournal::new(BlockConstants::dummy());
        let eoa = OriginalSource::new(
            RuntimeId::Ethereum,
            "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee".to_string(),
            Address::from([0xEE; 20]),
        );
        journal.set_original_source(eoa.clone());
        assert_eq!(journal.original_source(), Some(&eoa));

        journal.clear();
        assert_eq!(journal.original_source(), None);

        let next_eoa = OriginalSource::new(
            RuntimeId::Ethereum,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            Address::from([0xAA; 20]),
        );
        journal.set_original_source(next_eoa.clone());
        assert_eq!(journal.original_source(), Some(&next_eoa));
    }
}
