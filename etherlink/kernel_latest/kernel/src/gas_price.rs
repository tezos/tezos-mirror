// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Adjustments of the gas price (a.k.a `base_fee_per_gas`), in response to load.

use crate::block_in_progress::BlockInProgress;
use crate::transaction::Transaction;
use primitive_types::U256;
use softfloat::F64;
use tezos_ethereum::transaction::TransactionReceipt;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::timestamp::Timestamp;

const CAPACITY: u64 = 27_000_000;
const TARGET: u64 = CAPACITY / 2;
const TOLERANCE: u64 = 10 * TARGET;

// chosen so that gas price will decrease ~7/8 if there's no usage for ~10 seconds.
// ALPHA = -ln(7/8)/(TARGET * 10)
const ALPHA: F64 = softfloat::f64!(0.000_000_000_99);

/// Register a completed block into the tick backlog
pub fn register_block(
    host: &mut impl Runtime,
    bip: &BlockInProgress<Transaction, TransactionReceipt>,
) -> anyhow::Result<()> {
    if bip.queue_length() > 0 {
        anyhow::bail!("update_gas_price on non-empty block");
    }

    let cumulative_execution_gas = if bip.cumulative_execution_gas >= U256::from(u64::MAX)
    {
        u64::MAX
    } else {
        bip.cumulative_execution_gas.low_u64()
    };

    update_backlog(host, cumulative_execution_gas, bip.timestamp)?;

    Ok(())
}

/// Retrieve *base fee per gas*, according to the current timestamp.
pub fn base_fee_per_gas(
    host: &impl Runtime,
    timestamp: Timestamp,
    minimum_gas_price: U256,
) -> U256 {
    let timestamp = timestamp.as_u64();
    let minimum_gas_price = minimum_gas_price.as_u64();

    let last_timestamp =
        crate::storage::read_backlog_timestamp(host).unwrap_or(timestamp);

    let backlog = backlog_with_time_elapsed(host, 0, timestamp, last_timestamp);

    price_from_backlog(backlog, minimum_gas_price).into()
}

fn backlog_with_time_elapsed(
    host: &impl Runtime,
    extra_gas: u64,
    current_timestamp: u64,
    last_timestamp: u64,
) -> u64 {
    let diff = current_timestamp
        .saturating_sub(last_timestamp)
        .saturating_mul(TARGET);

    crate::storage::read_backlog(host)
        .unwrap_or_default()
        .saturating_sub(diff) // first take into account time elapsed
        .saturating_add(extra_gas) // then add the extra ticks just consumed
}

fn update_backlog(
    host: &mut impl Runtime,
    cumulative_execution_gas: u64,
    timestamp: Timestamp,
) -> anyhow::Result<()> {
    let timestamp = timestamp.as_u64();

    let last_timestamp_opt = crate::storage::read_backlog_timestamp(host).ok();
    let last_timestamp = last_timestamp_opt.unwrap_or(timestamp);

    if last_timestamp_opt.is_none() || timestamp > last_timestamp {
        crate::storage::store_backlog_timestamp(host, timestamp)?;
    }

    let backlog = backlog_with_time_elapsed(
        host,
        cumulative_execution_gas,
        timestamp,
        last_timestamp,
    );

    crate::storage::store_backlog(host, backlog)?;

    Ok(())
}

fn price_from_backlog(backlog: u64, minimum: u64) -> u64 {
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
    use primitive_types::H160;
    use proptest::prelude::*;
    use std::collections::VecDeque;
    use tezos_ethereum::block::BlockConstants;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};

    proptest! {
        #[test]
        fn f64_to_u64_conv(f in any::<f64>()) {
            assert_eq!(f as u64, f64_to_u64(f.into()));
        }

        #[test]
        fn u64_to_f64_conv(u in any::<u64>()) {
            let f: f64 = u64_to_f64(u).into();
            assert_eq!(u as f64, f);
        }

        #[test]
        fn gas_price(backlog in any::<u64>(), minimum in any::<u64>()) {
            let price = price_from_backlog(backlog, minimum);

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
        let mut host = MockKernelHost::default();
        let timestamp = 0_i64;
        let block_fees = crate::retrieve_block_fees(&mut host).unwrap();
        let dummy_block_constants = BlockConstants::first_block(
            timestamp.into(),
            U256::zero(),
            block_fees,
            crate::block::GAS_LIMIT,
            H160::zero(),
        );

        let mut bip = BlockInProgress::new_with_ticks(
            U256::zero(),
            Default::default(),
            VecDeque::new(),
            // estimated ticks in run (ignored)
            0,
            timestamp.into(),
            U256::zero(),
        );
        bip.cumulative_execution_gas = U256::from(TOLERANCE);

        register_block(&mut host, &bip).unwrap();
        bip.clone()
            .finalize_and_store(&mut host, &dummy_block_constants)
            .unwrap();

        // At tolerance, gas price should be min.
        let (min, gas_price) = load_gas_price(&mut host);
        assert_eq!(min, crate::fees::MINIMUM_BASE_FEE_PER_GAS.into());
        assert_eq!(gas_price, crate::fees::MINIMUM_BASE_FEE_PER_GAS.into());
        let gas_price_now = base_fee_per_gas(&host, timestamp.into(), min);
        assert_eq!(gas_price, gas_price_now);

        // register more blocks - now double tolerance
        bip.number = 1.into();
        register_block(&mut host, &bip).unwrap();
        bip.finalize_and_store(&mut host, &dummy_block_constants)
            .unwrap();
        let gas_price_now = base_fee_per_gas(&host, timestamp.into(), min);

        let (min, gas_price) = load_gas_price(&mut host);
        assert!(gas_price > min);
        assert_eq!(gas_price, gas_price_now);

        // after 10 seconds, reduces back to tolerance
        let gas_price_after_10 = base_fee_per_gas(&host, (timestamp + 10).into(), min);
        assert_eq!(
            gas_price_after_10,
            crate::fees::MINIMUM_BASE_FEE_PER_GAS.into()
        );
    }

    fn load_gas_price(host: &mut impl Runtime) -> (U256, U256) {
        let bf = crate::retrieve_block_fees(host).unwrap();

        (bf.minimum_base_fee_per_gas(), bf.base_fee_per_gas())
    }
}
