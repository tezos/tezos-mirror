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
    /// Cumulative gas used across all CRAC executions.
    pub gas_used: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EvmJournal {
    pub layered_state: LayeredState,
    pub inner: JournalInner<JournalEntry>,
    access_list: Option<AccessList>,
    /// Header info from the first incoming CRAC.
    crac_tx_info: Option<CracTransactionInfo>,
}

impl EvmJournal {
    pub fn new() -> Self {
        Self {
            layered_state: LayeredState::new(),
            inner: JournalInner::new(),
            access_list: None,
            crac_tx_info: None,
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

    /// Append logs and gas from a cross-runtime EVM execution.
    pub fn accumulate_crac_execution(&mut self, gas_used: u64) {
        if let Some(ref mut info) = self.crac_tx_info {
            info.gas_used = info.gas_used.saturating_add(gas_used);
        }
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
            gas_used: 0,
        };
        let info2 = CracTransactionInfo {
            source: Address::from([0x33; 20]),
            sender: Address::from([0x44; 20]),
            gas_limit: U256::from(2000),
            amount: U256::from(99),
            gas_used: 0,
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
        journal.accumulate_crac_execution(100);
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
                gas_used: 0,
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
                gas_used: 0,
            })
            .unwrap();
        journal.accumulate_crac_execution(100);

        let _ = journal.take_crac_data().unwrap();

        // Second call returns None (consumed)
        assert!(journal.take_crac_data().is_none());
        assert!(!journal.has_crac_data());
    }
}
