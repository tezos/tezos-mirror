// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;

/// Ceiling division of `a` by `b` on `U256`.
///
/// The caller must ensure `b != 0`.
pub fn ceil_div(a: U256, b: U256) -> U256 {
    match a.div_mod(b) {
        (quotient, remainder) if remainder.is_zero() => quotient,
        (quotient, _) => quotient.saturating_add(U256::one()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ceil_div_rounds_up_on_remainder() {
        assert_eq!(
            ceil_div(U256::from(4u64), U256::from(2u64)),
            U256::from(2u64)
        );
        assert_eq!(
            ceil_div(U256::from(5u64), U256::from(2u64)),
            U256::from(3u64)
        );
        assert_eq!(ceil_div(U256::zero(), U256::from(2u64)), U256::zero());
    }
}
