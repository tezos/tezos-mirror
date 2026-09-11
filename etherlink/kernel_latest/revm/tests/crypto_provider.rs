// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Asserts that the crypto provider backing revm's precompiles — the one that
//! `secp256k1_ecrecover` is dispatched to from `ec_recover_run` (upstream and
//! repriced) — is `EtherlinkCrypto` (libsecp256k1), not the default k256.
//!
//! Kept as a dedicated integration test so it runs in its own process: that
//! makes `ensure_etherlink_crypto_installed` the first `crypto()` touch, so it
//! deterministically wins the process-global `OnceLock`, unlike a unit test
//! sharing the process with other `crypto()` users.

use revm_etherlink::precompiles::crypto::ensure_etherlink_crypto_installed;

#[test]
fn installed_provider_is_etherlink_crypto() {
    assert!(
        ensure_etherlink_crypto_installed(),
        "ensure_etherlink_crypto_installed must win the install in a dedicated \
         test process"
    );
    // `crypto()` returns `&dyn Crypto`, which is `Debug`; the concrete type name
    // identifies the provider.
    let provider = format!("{:?}", revm::precompile::crypto());
    assert!(
        provider.contains("EtherlinkCrypto"),
        "the installed crypto() provider must be EtherlinkCrypto, got {provider:?}"
    );
}
