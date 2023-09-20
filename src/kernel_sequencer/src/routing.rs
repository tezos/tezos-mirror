// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

#[derive(Clone, Copy)]
pub enum FilterBehavior {
    /// All Internal/External messages will be added to the delayed inbox.
    AllowAll,
    /// All Internal messages will be added to the delayed inbox.
    /// Transfer will be added only if the destination is the current rollup.
    /// External messages have to use the framing protocol.
    OnlyThisRollup,
}

impl FilterBehavior {
    /// Indicate if a message has to be included in the delayed inbox or not.
    pub fn predicate(&self, payload: &[u8], rollup_address: &[u8]) -> bool {
        match self {
            FilterBehavior::AllowAll => true,
            FilterBehavior::OnlyThisRollup => match payload {
                [] => unreachable!("All messages contain an internal/external tag"),
                // If it's a transfer then the last n bytes should be the rollup address
                [0x00, 0x00, transfer @ ..] => transfer.ends_with(rollup_address),
                // All protocol-injected internal messages are sent to the kernel
                [0x00, ..] => true,
                // Base case for external message framing protocol
                [0x01, 0x00, remaining @ ..] => remaining.starts_with(rollup_address),
                // Unknown encoding
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::{BlockHash, ContractKt1Hash, HashTrait};
    use tezos_smart_rollup_encoding::{
        inbox::{InboxMessage, InfoPerLevel, InternalInboxMessage, Transfer},
        michelson::MichelsonUnit,
        public_key_hash::PublicKeyHash,
        smart_rollup::SmartRollupAddress,
        timestamp::Timestamp,
    };

    use super::FilterBehavior;

    fn address() -> SmartRollupAddress {
        SmartRollupAddress::from_b58check("sr1UNDWPUYVeomgG15wn5jSw689EJ4RNnVQa")
            .expect("decoding should work")
    }

    fn other_address() -> SmartRollupAddress {
        SmartRollupAddress::from_b58check("sr1UXY5i5Z1sF8xd8ZUyzur827MAaFWREzvj")
            .expect("decoding should work")
    }

    fn start_of_level() -> Vec<u8> {
        let start_of_level =
            InboxMessage::<MichelsonUnit>::Internal(InternalInboxMessage::StartOfLevel);
        let mut encoded = Vec::new();
        start_of_level
            .serialize(&mut encoded)
            .expect("encoding should work");
        encoded
    }

    fn end_of_level() -> Vec<u8> {
        let end_of_level =
            InboxMessage::<MichelsonUnit>::Internal(InternalInboxMessage::EndOfLevel);
        let mut encoded = Vec::new();
        end_of_level
            .serialize(&mut encoded)
            .expect("encoding should work");
        encoded
    }

    fn info_per_level() -> Vec<u8> {
        let predecessor =
            BlockHash::from_base58_check("BLockGenesisGenesisGenesisGenesisGenesisb83baZgbyZe")
                .expect("decoding should work");

        let info_per_level = InfoPerLevel {
            predecessor_timestamp: Timestamp::from(0),
            predecessor,
        };

        let info_per_level = InboxMessage::<MichelsonUnit>::Internal(
            InternalInboxMessage::InfoPerLevel(info_per_level),
        );
        let mut encoded = Vec::new();
        info_per_level
            .serialize(&mut encoded)
            .expect("encoding should work");
        encoded
    }

    fn transfer(rollup_address: &SmartRollupAddress) -> Vec<u8> {
        let transfer = Transfer {
            payload: MichelsonUnit {},
            sender: ContractKt1Hash::from_b58check("KT1NRLjyE7wxeSZ6La6DfuhSKCAAnc9Lnvdg")
                .expect("decoding should work"),
            source: PublicKeyHash::from_b58check("tz1bonDYXPijpBMA2kntUr87VqNe3oaLzpP1")
                .expect("decoding should work"),
            destination: rollup_address.clone(),
        };

        let transfer =
            InboxMessage::<MichelsonUnit>::Internal(InternalInboxMessage::Transfer(transfer));
        let mut encoded = Vec::new();
        transfer
            .serialize(&mut encoded)
            .expect("encoding should work");
        encoded
    }

    fn predicate(filter: FilterBehavior, msg: &[u8], rollup_address: &SmartRollupAddress) -> bool {
        let rollup_address_bytes = rollup_address.hash().as_ref();
        filter.predicate(msg, rollup_address_bytes)
    }

    #[test]
    fn test_allow_all_start_of_level() {
        assert!(predicate(
            FilterBehavior::AllowAll,
            &start_of_level(),
            &address(),
        ));
    }

    #[test]
    fn test_allow_all_end_of_level() {
        assert!(predicate(
            FilterBehavior::AllowAll,
            &end_of_level(),
            &address(),
        ));
    }

    #[test]
    fn test_allow_all_info_per_level() {
        assert!(predicate(
            FilterBehavior::AllowAll,
            &info_per_level(),
            &address(),
        ));
    }

    #[test]
    fn test_allow_all_transfer() {
        assert!(predicate(
            FilterBehavior::AllowAll,
            &transfer(&address()),
            &address(),
        ));
    }

    #[test]
    fn test_allow_all_external() {
        let external = [0x01, 0x0, 0x0];
        assert!(predicate(FilterBehavior::AllowAll, &external, &address(),));
    }

    #[test]
    fn test_only_this_rollup_start_of_level() {
        assert!(predicate(
            FilterBehavior::OnlyThisRollup,
            &start_of_level(),
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_info_per_level() {
        assert!(predicate(
            FilterBehavior::OnlyThisRollup,
            &info_per_level(),
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_end_of_level() {
        assert!(predicate(
            FilterBehavior::OnlyThisRollup,
            &end_of_level(),
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_accept_transfer() {
        assert!(predicate(
            FilterBehavior::OnlyThisRollup,
            &transfer(&address()),
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_refuse_transfer() {
        assert!(!predicate(
            FilterBehavior::OnlyThisRollup,
            &transfer(&other_address()),
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_accept_external() {
        // The external message starts by 0x01, 0x0 because we are using the framing protocol
        let mut external = vec![0x01, 0x0];
        let rollup_address = address();
        let mut rollup_address = rollup_address.hash().as_ref().clone();
        external.append(&mut rollup_address);

        assert!(predicate(
            FilterBehavior::OnlyThisRollup,
            &external,
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_refuse_external() {
        let mut external = vec![0x01, 0x0];
        let rollup_address = other_address();
        let mut rollup_address = rollup_address.hash().as_ref().clone();
        external.append(&mut rollup_address);

        assert!(!predicate(
            FilterBehavior::OnlyThisRollup,
            &external,
            &address(),
        ));
    }

    #[test]
    fn test_only_this_rollup_refuse_unknown_external_framing_protocol() {
        // Unknown framing protocol
        let mut external = vec![0x01, 0x99];
        let rollup_address = other_address();
        let mut rollup_address = rollup_address.hash().as_ref().clone();
        external.append(&mut rollup_address);

        assert!(!predicate(
            FilterBehavior::OnlyThisRollup,
            &external,
            &address(),
        ));
    }
}
