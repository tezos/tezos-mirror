// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fmt;
use std::num::NonZeroU64;
use std::ops::Add;
use std::ops::Deref;
use std::ops::Sub;

use crate::default::ConstDefault;
use crate::machine_state;

/// Virtual address
#[derive(
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize,
    derive_more::From,
)]
#[repr(transparent)]
pub struct VirtAddr(u64);

impl VirtAddr {
    /// Create a new virtual address.
    pub const fn new(addr: u64) -> Self {
        Self(addr)
    }

    /// Return an aligned address that is equal or lower than the current address.
    pub const fn align_down(self, align: NonZeroU64) -> Self {
        let overhang = self.0.rem_euclid(align.get());
        VirtAddr(self.0.saturating_sub(overhang))
    }

    /// Return an aligned address that is equal or higher than the current address.
    pub const fn align_up(self, align: NonZeroU64) -> Option<Self> {
        let overhang = self.0.rem_euclid(align.get());

        if overhang == 0 {
            return Some(self);
        }

        let underhang = align.get().saturating_sub(overhang);
        let Some(new) = self.0.checked_add(underhang) else {
            // Can't use ? operator in constant expression
            return None;
        };

        Some(VirtAddr(new))
    }

    /// Is the address a multiple of the given alignment?
    pub const fn is_aligned(self, align: NonZeroU64) -> bool {
        self.0.rem_euclid(align.get()) == 0
    }

    /// Convert the virtual address to the machine state's memory address representation.
    pub fn to_machine_address(self) -> machine_state::memory::Address {
        self.0
    }
}

impl ConstDefault for VirtAddr {
    const DEFAULT: Self = VirtAddr(u64::MAX);
}

impl fmt::Debug for VirtAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}

impl fmt::Display for VirtAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}

impl Add<u64> for VirtAddr {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        VirtAddr(self.0.wrapping_add(rhs))
    }
}

impl Sub for VirtAddr {
    type Output = i64;

    fn sub(self, rhs: Self) -> Self::Output {
        (self.0 as i64).wrapping_sub(rhs.0 as i64)
    }
}

impl Sub<u64> for VirtAddr {
    type Output = Self;

    fn sub(self, rhs: u64) -> Self::Output {
        VirtAddr(self.0.wrapping_sub(rhs))
    }
}

impl PartialEq<u64> for VirtAddr {
    fn eq(&self, other: &u64) -> bool {
        self.0.eq(other)
    }
}

impl PartialOrd<u64> for VirtAddr {
    fn partial_cmp(&self, other: &u64) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

/// Page-aligned value
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct PageAligned<T>(T);

impl<T> Deref for PageAligned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<u64> for PageAligned<VirtAddr> {
    type Error = super::Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        let addr = VirtAddr::new(value);

        if !addr.is_aligned(super::PAGE_SIZE) {
            return Err(super::Error::InvalidArgument);
        }

        Ok(PageAligned(addr))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_align_up() {
        proptest::proptest!(|(org_addr: u64, align: NonZeroU64)| {
            let org_addr = VirtAddr::new(org_addr);
            let addr = org_addr.align_up(align);

            match addr {
                Some(addr) => {
                    assert!(addr.is_aligned(align));
                    assert!(addr >= org_addr.0);
                }

                None => {
                    assert!(org_addr.to_machine_address() >= u64::MAX - align.get());
                }
            }
        });
    }

    #[test]
    fn test_align_down() {
        proptest::proptest!(|(org_addr: u64, align: NonZeroU64)| {
            let org_addr = VirtAddr::new(org_addr);
            let addr = org_addr.align_down(align);

            assert!(addr.is_aligned(align));
            assert!(addr <= org_addr.0);
        });
    }
}
