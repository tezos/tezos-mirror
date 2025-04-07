// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines PVM compilation targets in terms of the Rust compiler targets.

use std::{collections::HashSet, env};

/// Compilation target
pub struct Target {
    /// Expected `target_arch` configuration value
    target_arch: &'static str,

    // Expected `target_os` configuration value
    target_os: &'static str,

    /// Generated `pvm_kind` configuration value
    pvm_kind: &'static str,

    /// Generated `pvm_flavour` configuration value
    pvm_flavour: &'static str,
}

impl Target {
    // NOTE: Adding or amending the targets below might require updating Cargo manifests as well.
    // Cargo manifests can't make use of the target descriptions below. Therefore you must ensure
    // that Cargo manifests are amended inline with any modification to the PVM targets.

    /// RISC-V PVM with HermitOS
    pub const RISCV_HERMIT: Self = Self {
        target_arch: "riscv64",
        target_os: "hermit",
        pvm_kind: "riscv",
        pvm_flavour: "hermit",
    };

    /// RISC-V PVM without a bundled OS
    pub const RISCV_SUPERVISING: Self = Self {
        target_arch: "riscv64",
        target_os: "linux",
        pvm_kind: "riscv",
        pvm_flavour: "supervising",
    };

    /// WASM PVM
    pub const WASM: Self = Self {
        target_arch: "wasm32",
        target_os: "unknown",
        pvm_kind: "wasm",
        pvm_flavour: "none",
    };

    /// All PVM targets
    pub const ALL: [Self; 3] = [Self::RISCV_HERMIT, Self::RISCV_SUPERVISING, Self::WASM];

    /// Generate a meta item (i.e. body of a `#[cfg()]` annotation) that matches the given target
    /// description.
    #[cfg(feature = "macro-support")]
    pub fn to_meta(&self) -> proc_macro2::TokenStream {
        let Target {
            target_arch,
            target_os,
            ..
        } = self;

        quote::quote! {
            all(target_arch = #target_arch, target_os = #target_os)
        }
    }

    /// Generate a meta item (i.e. body of a `#[cfg()]` annotation) which match none of the
    /// supported PVM targets.
    #[cfg(feature = "macro-support")]
    pub fn fallback_meta() -> proc_macro2::TokenStream {
        let targets = Self::ALL.into_iter().map(|target| target.to_meta());
        quote::quote! {
            not(any(#(#targets),*))
        }
    }
}

/// Generate configuration aliases for the target PVM as part of a Cargo build script.
pub(crate) fn generate_cfg_aliases() {
    // With Rustc 1.80+ one can pass on a sanity check to the compiler to inform it that some
    // configuration values may be set. This is helpful in detecting impossible configurations.
    if rustc_version::version().unwrap()
        >= rustc_version::Version::parse("1.80.0").unwrap()
    {
        let mut kinds = HashSet::from(["none"]);
        let mut flavours = HashSet::from(["none"]);

        for target in Target::ALL {
            kinds.insert(target.pvm_kind);
            flavours.insert(target.pvm_flavour);
        }

        let kinds = kinds
            .into_iter()
            .map(|kind| format!("{kind:?}"))
            .collect::<Vec<_>>()
            .join(",");

        let flavours = flavours
            .into_iter()
            .map(|flavour| format!("{flavour:?}"))
            .collect::<Vec<_>>()
            .join(",");

        println!(r#"cargo:rustc-check-cfg=cfg(pvm_kind, values({kinds}))"#);
        println!(r#"cargo:rustc-check-cfg=cfg(pvm_flavour, values({flavours}))"#);
    }

    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();

    let (kind, flavour) = Target::ALL
        .into_iter()
        .find(|target| target.target_arch == target_arch && target.target_os == target_os)
        .map_or(("none", "none"), |target| {
            (target.pvm_kind, target.pvm_flavour)
        });

    println!("cargo:rustc-cfg=pvm_kind={kind:?}");
    println!("cargo:rustc-cfg=pvm_flavour={flavour:?}");
}
