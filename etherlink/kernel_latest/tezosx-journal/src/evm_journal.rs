// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;
use revm::{
    context::{transaction::AccessList, JournalInner},
    primitives::{Address, Log, U256},
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
    /// Logs accumulated from cross-runtime EVM executions (incoming CRACs).
    /// Multiple `serve()` calls within one foreign-runtime transaction
    /// append their logs here. Used to build the fake EVM transaction.
    ///
    /// Note: cross-runtime reverts for these logs are not handled here.
    /// See L2-1097.
    crac_logs: Vec<Log>,
    /// Header info from the first incoming CRAC.
    crac_tx_info: Option<CracTransactionInfo>,
}

impl EvmJournal {
    pub fn new() -> Self {
        Self {
            layered_state: LayeredState::new(),
            inner: JournalInner::new(),
            access_list: None,
            crac_logs: Vec::new(),
            crac_tx_info: None,
        }
    }
}

impl EvmJournal {
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
    pub fn accumulate_crac_execution(
        &mut self,
        logs: impl IntoIterator<Item = Log>,
        gas_used: u64,
    ) {
        self.crac_logs.extend(logs);
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
    pub fn take_crac_data(&mut self) -> Option<(Vec<Log>, CracTransactionInfo)> {
        let info = self.crac_tx_info.take()?;
        let logs = std::mem::take(&mut self.crac_logs);
        Some((logs, info))
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
    use revm::primitives::{Address, LogData};

    fn make_log(addr: [u8; 20], data: &[u8]) -> Log {
        Log {
            address: Address::from(addr),
            data: LogData::new_unchecked(vec![], data.to_vec().into()),
        }
    }

    #[test]
    fn test_accumulate_crac_logs() {
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
        let log1 = make_log([0x11; 20], b"log1");
        let log2 = make_log([0x22; 20], b"log2");

        journal.accumulate_crac_execution(vec![log1.clone()], 100);
        journal.accumulate_crac_execution(vec![log2.clone()], 200);

        assert!(journal.has_crac_data());
        let (logs, _) = journal.take_crac_data().unwrap();
        assert_eq!(logs.len(), 2);
    }

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
        journal.accumulate_crac_execution(vec![make_log([0x11; 20], b"x")], 100);
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
        let (logs, _) = journal.take_crac_data().unwrap();
        assert!(logs.is_empty());
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
        journal.accumulate_crac_execution(vec![make_log([0x11; 20], b"x")], 100);

        let (logs, _) = journal.take_crac_data().unwrap();
        assert_eq!(logs.len(), 1);

        // Second call returns None (consumed)
        assert!(journal.take_crac_data().is_none());
        assert!(!journal.has_crac_data());
    }
}
