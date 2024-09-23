// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(unused_imports)]

cfg_if::cfg_if! {
    if #[cfg(target_arch = "wasm32")] {
        mod wasm;
        pub use wasm::*;
    } else if #[cfg(all(target_arch = "riscv64", target_os = "hermit"))] {
        mod riscv;
        pub use riscv::*;
    } else {
        mod native;
        pub use native::*;
    }
}
