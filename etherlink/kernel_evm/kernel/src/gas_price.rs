// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Adjustments of the gas price (a.k.a `base_fee_per_gas`), in response to load.

use crate::block_in_progress::BlockInProgress;
use crate::storage::{
    read_base_fee_per_gas, read_minimum_base_fee_per_gas, store_base_fee_per_gas,
};

use primitive_types::U256;
use softfloat::F64;
use tezos_ethereum::block::BlockFees;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::runtime::Runtime;

// actual ~34M, allow some overhead for less effecient ERC20 transfers.
const ERC20_TICKS: u64 = 40_000_000;
// 50 TPS of ERC20 transfers should be sustained without increasing price.
const SPEED_LIMIT: u64 = 50 * ERC20_TICKS;
const TOLERANCE: u64 = 10 * SPEED_LIMIT;

// chosen so that gas price will decrease ~7/8 if there's no usage for ~10 seconds.
// ALPHA = -ln(7/8)/(SPEED_LIMIT * 10)
const ALPHA: F64 = softfloat::f64!(0.000_000_000_007);

/// Load the current base fee per gas from storage.
pub fn load_gas_price(host: &mut impl Runtime) -> Result<U256, crate::Error> {
    let minimum_base_fee_per_gas = read_minimum_base_fee_per_gas(host)
        .unwrap_or_else(|_| crate::fees::MINIMUM_BASE_FEE_PER_GAS.into());

    let base_fee_per_gas = match read_base_fee_per_gas(host) {
        Ok(base_fee_per_gas) if base_fee_per_gas > minimum_base_fee_per_gas => {
            base_fee_per_gas
        }
        _ => {
            store_base_fee_per_gas(host, minimum_base_fee_per_gas)?;
            minimum_base_fee_per_gas
        }
    };

    Ok(base_fee_per_gas)
}

/// Register a completed block into the tick backlog
pub fn register_block(
    host: &mut impl Runtime,
    bip: &BlockInProgress,
) -> anyhow::Result<()> {
    if bip.queue_length() > 0 {
        anyhow::bail!("update_gas_price on non-empty block");
    }

    update_tick_backlog(host, bip.estimated_ticks_in_block, bip.timestamp)
}

/// Update the kernel-wide base fee per gas with a new value.
pub fn store_new_base_fee_per_gas(
    host: &mut impl Runtime,
    gas_price: U256,
    constants: &mut BlockFees,
) -> anyhow::Result<()> {
    crate::storage::store_base_fee_per_gas(host, gas_price)?;
    constants.set_base_fee_per_gas(gas_price);
    Ok(())
}

/// Retrieve *base fee per gas*, according to the current timestamp.
pub fn base_fee_per_gas(host: &impl Runtime, timestamp: Timestamp) -> U256 {
    let timestamp = timestamp.as_u64();

    let last_timestamp =
        crate::storage::read_tick_backlog_timestamp(host).unwrap_or(timestamp);

    let backlog = backlog_with_time_elapsed(host, 0, timestamp, last_timestamp);

    let minimum_gas_price = read_minimum_base_fee_per_gas(host)
        .map(|p| p.as_u64())
        .unwrap_or(crate::fees::MINIMUM_BASE_FEE_PER_GAS);

    price_from_tick_backlog(backlog, minimum_gas_price).into()
}

fn backlog_with_time_elapsed(
    host: &impl Runtime,
    extra_ticks: u64,
    current_timestamp: u64,
    last_timestamp: u64,
) -> u64 {
    let diff = current_timestamp
        .saturating_sub(last_timestamp)
        .saturating_mul(SPEED_LIMIT);

    crate::storage::read_tick_backlog(host)
        .unwrap_or_default()
        .saturating_sub(diff) // first take into account time elapsed
        .saturating_add(extra_ticks) // then add the extra ticks just consumed
}

fn update_tick_backlog(
    host: &mut impl Runtime,
    ticks_in_block: u64,
    timestamp: Timestamp,
) -> anyhow::Result<()> {
    let timestamp = timestamp.as_u64();

    let last_timestamp_opt = crate::storage::read_tick_backlog_timestamp(host).ok();
    let last_timestamp = last_timestamp_opt.unwrap_or(timestamp);

    if last_timestamp_opt.is_none() || timestamp > last_timestamp {
        crate::storage::store_tick_backlog_timestamp(host, timestamp)?;
    }

    let backlog =
        backlog_with_time_elapsed(host, ticks_in_block, timestamp, last_timestamp);

    crate::storage::store_tick_backlog(host, backlog)?;

    Ok(())
}

fn price_from_tick_backlog(backlog: u64, minimum: u64) -> u64 {
    if backlog <= TOLERANCE {
        return minimum;
    }

    let min = u64_to_f64(minimum);

    let price = min * F64::exp(ALPHA * u64_to_f64(backlog - TOLERANCE));

    let price = f64_to_u64(price);

    if price < minimum {
        minimum
    } else {
        price
    }
}

fn u64_to_f64(i: u64) -> F64 {
    F64::from_bits(u64_to_f64_bits(i))
}

// compiler builtins
// https://github.com/rust-lang/compiler-builtins/blob/351d48e4b95f1665cfd3360e3ba8f3dd4d3fb3c1/src/float/conv.rs
//
// SPDX-License-Identifier: MIT
fn u64_to_f64_bits(i: u64) -> u64 {
    if i == 0 {
        return 0;
    }
    let n = i.leading_zeros();
    let a = (i << n) >> 11; // Significant bits, with bit 53 still in tact.
    let b = (i << n) << 53; // Insignificant bits, only relevant for rounding.
    let m = a + ((b - (b >> 63 & !a)) >> 63); // Add one when we need to round up. Break ties to even.
    let e = 1085 - n as u64; // Exponent plus 1023, minus one.
    (e << 52) + m // + not |, so the mantissa can overflow into the exponent.
}

fn f64_to_u64(f: F64) -> u64 {
    let fbits = f.to_bits();
    if fbits < 1023 << 52 {
        // >= 0, < 1
        0
    } else if fbits < 1087 << 52 {
        // >= 1, < max
        let m = 1 << 63 | fbits << 11; // Mantissa and the implicit 1-bit.
        let s = 1086 - (fbits >> 52); // Shift based on the exponent and bias.
        m >> s
    } else if fbits <= 2047 << 52 {
        // >= max (incl. inf)
        u64::MAX
    } else {
        // Negative or NaN
        0
    }
}
// end 'compiler builtins'

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;
    use std::collections::VecDeque;
    use tezos_smart_rollup_mock::MockHost;

    proptest! {
        #[test]
        fn f64_to_u64_conv(f in any::<f64>()) {
            assert_eq!(f as u64, f64_to_u64(f.into()));
        }

        #[test]
        fn u64_to_f64_conv(u in any::<u64>()) {
            assert_eq!(u as f64, u64_to_f64(u).into());
        }

        #[test]
        fn gas_price(backlog in any::<u64>(), minimum in any::<u64>()) {
            let price = price_from_tick_backlog(backlog, minimum);

            if backlog <= TOLERANCE {
                assert_eq!(price, minimum);
            } else {
                let exponent = 0.000_000_000_007_f64 * (backlog - TOLERANCE) as f64;
                let expected = (minimum as f64) * f64::exp(exponent);
                assert_eq!(expected as u64, price);
            }
        }
    }

    #[test]
    fn gas_price_responds_to_load() {
        let mut host = MockHost::default();
        let timestamp = 0_i64;
        let mut block_fees = BlockFees::new(U256::zero(), U256::zero());

        let mut bip = BlockInProgress::new_with_ticks(
            U256::zero(),
            Default::default(),
            U256::zero(),
            VecDeque::new(),
            // estimated ticks in run (ignored)
            0,
            timestamp.into(),
        );
        bip.estimated_ticks_in_block = TOLERANCE;

        register_block(&mut host, &bip).unwrap();

        // At tolerance, gas price should be min.
        let gas_price = load_gas_price(&mut host).unwrap();
        assert_eq!(gas_price, crate::fees::MINIMUM_BASE_FEE_PER_GAS.into());
        let gas_price_now = base_fee_per_gas(&host, timestamp.into());
        assert_eq!(gas_price, gas_price_now);

        // register more blocks - now double tolerance
        register_block(&mut host, &bip).unwrap();
        let gas_price_now = base_fee_per_gas(&host, timestamp.into());
        store_new_base_fee_per_gas(&mut host, gas_price_now, &mut block_fees).unwrap();

        let gas_price = load_gas_price(&mut host).unwrap();
        assert!(gas_price > crate::fees::MINIMUM_BASE_FEE_PER_GAS.into());
        assert_eq!(gas_price, gas_price_now);

        // after 10 seconds, reduces back to tolerance
        let gas_price_after_10 = base_fee_per_gas(&host, (timestamp + 10).into());
        assert_eq!(
            gas_price_after_10,
            crate::fees::MINIMUM_BASE_FEE_PER_GAS.into()
        );
    }
}
