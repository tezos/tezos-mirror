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

#[derive(Debug, PartialEq, Eq)]
pub struct EvmJournal {
    pub layered_state: LayeredState,
    pub inner: JournalInner<JournalEntry>,
    access_list: Option<AccessList>,
    /// Header info from the first incoming CRAC.
    crac_tx_info: Option<CracTransactionInfo>,
    /// Original EVM source (E_0 / `tx.origin`) of the current
    /// top-level EVM transaction.  Set on the first outgoing
    /// EVM→Michelson gateway call and re-read on every subsequent
    /// gateway call — including ones issued by re-entrant EVM frames
    /// spawned by Michelson `call_evm`.  Lives on `EvmJournal`
    /// (which persists for the whole transaction across runtime
    /// crossings) rather than on the per-revm-execution `Journal`
    /// wrapper, so re-entry preserves it instead of falling back to
    /// the inner frame's `tx().caller()` (which would be the alias
    /// of the Michelson contract that called `call_evm`, not the
    /// EOA).
    original_evm_source: Option<Address>,
}

impl EvmJournal {
    pub fn new() -> Self {
        Self {
            layered_state: LayeredState::new(),
            inner: JournalInner::new(),
            access_list: None,
            crac_tx_info: None,
            original_evm_source: None,
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
        self.original_evm_source = None;
    }

    /// Retrieve the original EVM source captured on the first
    /// outgoing gateway call of this transaction, if any.
    pub fn original_evm_source(&self) -> Option<Address> {
        self.original_evm_source
    }

    /// Capture the original EVM source on the first outgoing gateway
    /// call.  Subsequent calls are no-ops so re-entrant frames cannot
    /// overwrite the EOA with an intermediate alias.
    pub fn set_original_evm_source(&mut self, source: Address) {
        if self.original_evm_source.is_none() {
            self.original_evm_source = Some(source);
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
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use revm::primitives::Address;

    #[test]
    fn test_set_crac_tx_info_errors_on_duplicate() {
        let mut journal = EvmJournal::new();
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
        let mut journal = EvmJournal::new();
        assert!(journal.take_crac_data().is_none());
    }

    #[test]
    fn test_take_crac_data_returns_none_without_tx_info() {
        let mut journal = EvmJournal::new();
        // No tx info set → returns None
        assert!(journal.take_crac_data().is_none());
    }

    #[test]
    fn test_take_crac_data_with_info_but_no_logs() {
        let mut journal = EvmJournal::new();
        journal
            .set_crac_tx_info(CracTransactionInfo {
                source: Address::from([0x11; 20]),
                sender: Address::from([0x22; 20]),
                gas_limit: U256::from(1000),
                amount: U256::ZERO,
            })
            .unwrap();
        // No logs but tx info is set → returns Some with empty logs
        let _ = journal.take_crac_data().unwrap();
        assert!(!journal.has_crac_data());
    }

    #[test]
    fn test_take_crac_data_consumes() {
        let mut journal = EvmJournal::new();
        journal
            .set_crac_tx_info(CracTransactionInfo {
                source: Address::from([0x11; 20]),
                sender: Address::from([0x22; 20]),
                gas_limit: U256::from(1000),
                amount: U256::ZERO,
            })
            .unwrap();

        let _ = journal.take_crac_data().unwrap();

        // Second call returns None (consumed)
        assert!(journal.take_crac_data().is_none());
        assert!(!journal.has_crac_data());
    }

    // `original_evm_source` starts unset on a fresh journal.
    #[test]
    fn test_original_evm_source_starts_unset() {
        let journal = EvmJournal::new();
        assert_eq!(journal.original_evm_source(), None);
    }

    // The first call to `set_original_evm_source` captures the EOA;
    // subsequent calls are no-ops so a re-entrant EVM frame's
    // `tx().caller()` (which would be the alias of the Michelson
    // contract that called `call_evm`, not the EOA) cannot overwrite
    // the originally-captured value.
    #[test]
    fn test_original_evm_source_first_set_wins() {
        let mut journal = EvmJournal::new();
        let eoa = Address::from([0xEE; 20]);
        let intermediate_alias = Address::from([0xCC; 20]);

        journal.set_original_evm_source(eoa);
        assert_eq!(journal.original_evm_source(), Some(eoa));

        // Simulate the re-entrant frame's first gateway call trying
        // to set its own (incorrect) source — must be ignored.
        journal.set_original_evm_source(intermediate_alias);
        assert_eq!(journal.original_evm_source(), Some(eoa));
    }

    // `clear()` resets `original_evm_source` so the next operation in
    // the same kernel iteration starts with a fresh capture window.
    #[test]
    fn test_clear_resets_original_evm_source() {
        let mut journal = EvmJournal::new();
        let eoa = Address::from([0xEE; 20]);
        journal.set_original_evm_source(eoa);
        assert_eq!(journal.original_evm_source(), Some(eoa));

        journal.clear();
        assert_eq!(journal.original_evm_source(), None);

        // After clear, a new capture should land normally.
        let next_eoa = Address::from([0xAA; 20]);
        journal.set_original_evm_source(next_eoa);
        assert_eq!(journal.original_evm_source(), Some(next_eoa));
    }
}
