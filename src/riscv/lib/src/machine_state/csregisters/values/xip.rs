// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::csregisters::effects::CSREffect;
use crate::state_backend::AllocatedOf;
use crate::state_backend::ManagerBase;

/// Layout for [`XipCell`]
pub type XipCellLayout = ();

/// Dummy cell for MIP and SIP (effectively always 0)
#[derive(Clone)]
pub struct XipCell;

impl XipCell {
    /// Bind the given allocated regions.
    pub fn bind<M: ManagerBase>(_space: AllocatedOf<XipCellLayout, M>) -> Self {
        XipCell
    }

    /// Does nothing.
    pub fn struct_ref(&self) {}

    /// Always returns 0.
    #[inline(always)]
    pub const fn read(&self) -> u64 {
        0
    }

    /// Does nothing but trigger the XIP effect.
    #[inline(always)]
    pub fn write(&mut self, _: u64) -> Option<CSREffect> {
        Some(CSREffect::Xip)
    }

    /// Does nothing but trigger the XIP effect.
    #[inline(always)]
    pub fn replace(&mut self, _: u64) -> (u64, Option<CSREffect>) {
        (0, Some(CSREffect::Xip))
    }
}
