// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! JIT-compiled blocks must be able to interact with the
//! RISC-V [`MachineCoreState`] passed to them.

use crate::state_backend::{owned_backend::Owned, ManagerReadWrite};

/// State Access that a JIT-compiled block may use.
///
/// In future, this will come in two parts:
/// - `extern "C"` functions that can be registered in the JIT module
/// - a way of calling those functions from within JIT-compiled code
// TODO (RV-403, RV-404): add functionality to
//  - update pc
//  - read/write to xregisters
pub trait JitStateAccess: ManagerReadWrite {}

impl JitStateAccess for Owned {}
