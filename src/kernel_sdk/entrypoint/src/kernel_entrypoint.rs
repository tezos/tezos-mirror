// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(unused_imports)]

cfg_if::cfg_if! {
    if #[cfg(pvm_kind = "wasm")] {
        mod wasm;
        pub use wasm::*;
    } else if #[cfg(pvm_kind = "riscv")] {
        mod riscv;
        pub use riscv::*;
    } else if #[cfg(pvm_kind = "none")] {
        mod native;
        pub use native::*;
    }
}
