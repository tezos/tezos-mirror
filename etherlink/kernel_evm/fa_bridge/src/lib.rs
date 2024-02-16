// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

//! FA token bridge.
//!
//! A permissionless transport protocol, that enables ticket transfers
//! from L1 to L2 and back, supporting two destination types:
//!     1. Simple address, which can be both externally owner account,
//!        or a smart contract wallet (that supports tickets)
//!     2. Proxy contract, exposing standard methods for deposits (on L2)
//!        and withdrawals (on L1); must handle both ticket and
//!        routing info that carries the final receiver address.
//!
//! FA bridge maintains the global ticket table, which is a ledger
//! tracking internal ticket ownerships on Etherlink side.
//!
//! FA bridge consists of two main parts:
//!     * The one responsible for deposit handling: integrates with the
//!       inbox handling flow, results in a pseudo transaction from
//!       Zero account.
//!     * The one responsible for withdrawal handling: implemented as
//!       as precompiled contract, which can be invoked both by EOA
//!       or another smart contract.
//!
//! It should be noted that FA withdrawal precompile DOES NOT post any
//! messages to the outbox since it cannot know if the outer transaction
//! fails or succeeds.
//!
//! All the state updates (ticket table, outbox message counter) are done
//! using the transactional Eth account storage, so that they are discarded
//! in case of a revert/failure.
