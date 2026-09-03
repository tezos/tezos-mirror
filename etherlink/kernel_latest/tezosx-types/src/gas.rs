//! Gas for cross-runtime calls: the [`Gas`] type and the unit
//! conversions behind it.
//!
//! **Convention**: `X-Tezos-Gas-Limit` and `X-Tezos-Gas-Consumed` both
//! carry milligas, the finest unit, whichever runtimes are talking. A
//! sender therefore needs to know nothing about its target's unit, and a
//! receiver nothing about its caller's: each converts into its own on
//! arrival. [`Display`] and [`FromStr`] are that wire form, and the only
//! way a [`Gas`] becomes a header value or comes back from one.
//!
//! **Ratio**: 1 EVM gas = [`tezosx_constants::EVM_GAS_TO_MILLIGAS`] Tezos
//! milligas.
//! **Tezos unit**: milligas (the native unit of the Tezos runtime).

use std::{
    fmt::{Debug, Display},
    ops::{Add, Mul, Sub},
    str::FromStr,
};

use crate::RuntimeId;
use tezosx_constants::EVM_GAS_TO_MILLIGAS;

/// Convert `gas` from `source` runtime units to `target` runtime units.
fn convert(source: RuntimeId, target: RuntimeId, gas: u64) -> u64 {
    match (source, target) {
        (RuntimeId::Ethereum, RuntimeId::Tezos) => {
            gas.saturating_mul(EVM_GAS_TO_MILLIGAS)
        }
        (RuntimeId::Tezos, RuntimeId::Ethereum) => gas / EVM_GAS_TO_MILLIGAS,
        _ => gas,
    }
}

/// Like [`convert`], but rounds UP.
///
/// This is the rounding every [`Gas`] conversion uses, budgets included —
/// see [`Gas::as_runtime`] for why a single direction suffices and why it
/// is this one.
fn convert_ceil(source: RuntimeId, target: RuntimeId, gas: u64) -> u64 {
    match (source, target) {
        (RuntimeId::Tezos, RuntimeId::Ethereum) => gas.div_ceil(EVM_GAS_TO_MILLIGAS),
        _ => convert(source, target, gas),
    }
}

/// An amount of gas, together with the runtime whose unit it is expressed
/// in (EVM gas for Ethereum, milligas for Tezos).
///
/// Carrying the unit in the type is what makes the cross-runtime metering
/// paths auditable: a value can no longer be handed to the wrong runtime
/// without an explicit [`Gas::as_runtime`] at the boundary, and the manual
/// `convert` calls that used to sit at every such boundary disappear.
///
/// Conversions saturate instead of failing, and both directions are safe:
/// a saturated *budget* only ever over-states what a callee may spend (the
/// caller still charges what was actually consumed), while a saturated
/// *charge* is `u64::MAX`, i.e. an immediate out-of-gas. Either bound needs
/// more than `u64::MAX / EVM_GAS_TO_MILLIGAS` gas, far beyond what a block
/// can hold.
#[derive(Copy, Clone)]
pub struct Gas {
    gas: u64,
    runtime: RuntimeId,
}

impl Gas {
    /// No gas. Comparisons and conversions are unit-agnostic at zero, so
    /// the runtime tag carried here is immaterial.
    pub const ZERO: Gas = Gas::new(0, RuntimeId::Tezos);

    /// Create a new `Gas` holding `gas` units of `runtime`.
    pub const fn new(gas: u64, runtime: RuntimeId) -> Self {
        Self { gas, runtime }
    }

    /// The amount, expressed in `runtime`'s unit.
    ///
    /// The single conversion in the type, and it rounds *up* when the
    /// target unit is coarser (Tezos → Ethereum). Rounding up is the
    /// direction that never favours the holder: a charge never
    /// under-covers what was consumed, and a budget lends out a sub-unit
    /// remainder — at most `EVM_GAS_TO_MILLIGAS - 1` — that [`Sub`] bills
    /// straight back.
    pub fn as_runtime(&self, runtime: RuntimeId) -> u64 {
        if self.runtime == runtime {
            self.gas
        } else {
            convert_ceil(self.runtime, runtime, self.gas)
        }
    }

    /// Exact value in the finest unit (Tezos milligas).
    /// Comparing there keeps [`Ord`] total
    /// and symmetric: the Ethereum → Tezos direction is exact, whereas
    /// comparing in EVM gas would make two milligas amounts within the
    /// same 1-gas bucket compare equal.
    fn in_milligas(&self) -> u64 {
        convert(self.runtime, RuntimeId::Tezos, self.gas)
    }
}

/// A gas amount in the Michelson runtime's native unit.
///
/// Exists so that a function metering in milligas can *say so in its
/// signature*: the unit is then inferred at the call site from what the
/// callee asks for, instead of being spelled out with a [`RuntimeId`] the
/// caller has to get right.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Milligas(u64);

/// A gas amount in the EVM runtime's native unit. Counterpart of
/// [`Milligas`].
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EvmGas(u64);

macro_rules! native_unit {
    ($ty:ident, $runtime:expr) => {
        impl $ty {
            pub const ZERO: $ty = $ty(0);

            pub const fn new(gas: u64) -> Self {
                Self(gas)
            }

            pub const fn is_zero(self) -> bool {
                self.0 == 0
            }
        }

        impl From<Gas> for $ty {
            fn from(gas: Gas) -> Self {
                $ty(gas.as_runtime($runtime))
            }
        }

        impl From<$ty> for Gas {
            fn from(amount: $ty) -> Self {
                Gas::new(amount.0, $runtime)
            }
        }

        /// The exit into a raw meter. Deliberately not `Deref`: leaving the
        /// typed world is spelled out at the call site.
        impl From<$ty> for u64 {
            fn from(amount: $ty) -> u64 {
                amount.0
            }
        }

        /// Saturating, like every other conversion here: a cost overflow is
        /// an immediate out-of-gas, never a wrap-around.
        impl Add for $ty {
            type Output = Self;

            fn add(self, rhs: Self) -> Self {
                $ty(self.0.saturating_add(rhs.0))
            }
        }

        /// `rate * count`, for the per-word and per-header surcharges.
        impl Mul<u64> for $ty {
            type Output = Self;

            fn mul(self, count: u64) -> Self {
                $ty(self.0.saturating_mul(count))
            }
        }

        impl Mul<$ty> for u64 {
            type Output = $ty;

            fn mul(self, rate: $ty) -> $ty {
                rate * self
            }
        }
    };
}

native_unit!(Milligas, RuntimeId::Tezos);
native_unit!(EvmGas, RuntimeId::Ethereum);

impl PartialEq for Gas {
    fn eq(&self, other: &Self) -> bool {
        self.in_milligas() == other.in_milligas()
    }
}

impl Eq for Gas {}

impl PartialOrd for Gas {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Gas {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.in_milligas().cmp(&other.in_milligas())
    }
}

impl Sub for Gas {
    type Output = Self;

    /// The difference is measured in the *right-hand side's* unit — the
    /// unit the subtracted amount was actually metered in — and the result
    /// carries that unit.
    ///
    /// This is what keeps `budget - remaining` honest across a runtime
    /// boundary. Converting a budget into a coarser unit rounds up, so the
    /// callee is lent up to `EVM_GAS_TO_MILLIGAS - 1` of the caller's finer
    /// unit. Measuring the difference in the caller's unit would subtract
    /// the callee's coarse figure from the *un-rounded* budget and silently
    /// credit that loan back; measuring it in the callee's unit bills the
    /// loan in full, so an over-spending callee drives the caller out of gas
    /// instead of getting the work for free.
    ///
    /// A runtime handed a budget must never report more gas remaining than
    /// it was given. If one does, the kernel must not panic — but it must
    /// not hand out the work for free either, so the difference clamps to
    /// the *whole budget* rather than to zero: the caller is billed
    /// everything it lent, which is the conservative direction.
    fn sub(self, rhs: Self) -> Self::Output {
        let budget = self.as_runtime(rhs.runtime);
        debug_assert!(
            rhs.gas <= budget,
            "a runtime reported {} gas remaining out of a budget of {budget}",
            rhs.gas
        );
        Self {
            gas: budget.checked_sub(rhs.gas).unwrap_or(budget),
            runtime: rhs.runtime,
        }
    }
}

impl Debug for Gas {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} gas in {:?}", self.gas, self.runtime)
    }
}

/// The canonical wire form: the amount in the finest unit (Tezos
/// milligas), and nothing else.
///
/// This is what `X-Tezos-Gas-Limit` and `X-Tezos-Gas-Consumed` carry, so
/// neither side of a cross-runtime call has to know the other's unit.
/// `Display` *is* that format — rather than a second `serialize` method
/// beside a human-readable `Display` — so that writing a `Gas` into a
/// header cannot accidentally emit prose. The readable rendering lives on
/// [`Debug`].
impl Display for Gas {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.in_milligas())
    }
}

impl FromStr for Gas {
    type Err = std::num::ParseIntError;

    /// Parses the canonical wire form produced by [`Display`].
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Gas::new(s.parse::<u64>()?, RuntimeId::Tezos))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ethereum_to_tezos() {
        assert_eq!(
            convert(RuntimeId::Ethereum, RuntimeId::Tezos, 100),
            100 * EVM_GAS_TO_MILLIGAS
        );
        assert_eq!(
            convert(RuntimeId::Ethereum, RuntimeId::Tezos, 1_000_000),
            1_000_000 * EVM_GAS_TO_MILLIGAS
        );
    }

    #[test]
    fn ethereum_to_tezos_overflow() {
        let large = u64::MAX / EVM_GAS_TO_MILLIGAS + 1;
        assert_eq!(
            convert(RuntimeId::Ethereum, RuntimeId::Tezos, large),
            u64::MAX
        );
    }

    #[test]
    fn tezos_to_ethereum() {
        assert_eq!(
            convert(
                RuntimeId::Tezos,
                RuntimeId::Ethereum,
                100 * EVM_GAS_TO_MILLIGAS
            ),
            100
        );
    }

    #[test]
    fn tezos_to_ethereum_truncates() {
        assert_eq!(
            convert(
                RuntimeId::Tezos,
                RuntimeId::Ethereum,
                EVM_GAS_TO_MILLIGAS + EVM_GAS_TO_MILLIGAS / 2
            ),
            1
        );
    }

    #[test]
    fn identity() {
        assert_eq!(convert(RuntimeId::Ethereum, RuntimeId::Ethereum, 42), 42);
        assert_eq!(convert(RuntimeId::Tezos, RuntimeId::Tezos, 42), 42);
    }

    #[test]
    fn zero() {
        assert_eq!(convert(RuntimeId::Ethereum, RuntimeId::Tezos, 0), 0);
        assert_eq!(convert(RuntimeId::Tezos, RuntimeId::Ethereum, 0), 0);
    }

    #[test]
    fn tezos_to_ethereum_ceil_rounds_up() {
        // 944 = 22 * 42 + 20 (the %collect_result witness from L2-1751).
        assert_eq!(convert_ceil(RuntimeId::Tezos, RuntimeId::Ethereum, 944), 43);
        // Exact multiple: no over-charge.
        assert_eq!(
            convert_ceil(
                RuntimeId::Tezos,
                RuntimeId::Ethereum,
                EVM_GAS_TO_MILLIGAS * 42
            ),
            42
        );
        assert_eq!(convert_ceil(RuntimeId::Tezos, RuntimeId::Ethereum, 1), 1);
        assert_eq!(convert_ceil(RuntimeId::Tezos, RuntimeId::Ethereum, 0), 0);
    }

    #[test]
    fn ceil_other_paths_match_convert() {
        assert_eq!(
            convert_ceil(RuntimeId::Ethereum, RuntimeId::Tezos, 100),
            100 * EVM_GAS_TO_MILLIGAS
        );
        assert_eq!(
            convert_ceil(RuntimeId::Ethereum, RuntimeId::Ethereum, 42),
            42
        );
    }
}

#[cfg(test)]
mod gas_type_tests {
    use super::*;

    const ETH: RuntimeId = RuntimeId::Ethereum;
    const TEZ: RuntimeId = RuntimeId::Tezos;

    #[test]
    fn as_runtime_is_identity_in_its_own_unit() {
        assert_eq!(Gas::new(42, ETH).as_runtime(ETH), 42);
        assert_eq!(Gas::new(42, TEZ).as_runtime(TEZ), 42);
    }

    #[test]
    fn as_runtime_widens_exactly() {
        // Ethereum -> Tezos multiplies: no rounding can occur.
        assert_eq!(Gas::new(42, ETH).as_runtime(TEZ), 42 * EVM_GAS_TO_MILLIGAS);
    }

    #[test]
    fn as_runtime_rounds_up_into_the_coarser_unit() {
        // Tezos -> Ethereum divides, and is the only place a rounding
        // decision exists. It rounds up, so a converted charge never
        // under-covers and a converted budget never over-states what the
        // holder can be billed for.
        // 944 = 22 * 42 + 20 (the %collect_result witness from L2-1751).
        assert_eq!(Gas::new(944, TEZ).as_runtime(ETH), 43);
        // Exact multiple: no over-charge.
        assert_eq!(Gas::new(42 * EVM_GAS_TO_MILLIGAS, TEZ).as_runtime(ETH), 42);
    }

    #[test]
    fn as_runtime_saturates_instead_of_overflowing() {
        let huge = Gas::new(u64::MAX / EVM_GAS_TO_MILLIGAS + 1, ETH);
        assert_eq!(huge.as_runtime(TEZ), u64::MAX);
    }

    #[test]
    fn equality_is_unit_agnostic() {
        assert_eq!(Gas::new(1, ETH), Gas::new(EVM_GAS_TO_MILLIGAS, TEZ));
        assert_eq!(Gas::ZERO, Gas::new(0, ETH));
        // Amounts inside the same 1-EVM-gas bucket stay distinct: comparing
        // in EVM gas would have collapsed these two.
        assert_ne!(Gas::new(1, TEZ), Gas::new(2, TEZ));
    }

    #[test]
    fn ordering_is_symmetric_across_units() {
        let coarse = Gas::new(1, ETH); // 22 milligas
        let fine = Gas::new(21, TEZ);
        assert!(fine < coarse);
        assert!(coarse > fine);
        // `Ord` and `PartialEq` agree, as their contract requires.
        assert_eq!(
            Gas::new(1, ETH).cmp(&Gas::new(EVM_GAS_TO_MILLIGAS, TEZ)),
            std::cmp::Ordering::Equal
        );
    }

    #[test]
    fn wire_form_is_canonical_milligas() {
        // Both units serialise to the same string, so neither side of a
        // cross-runtime call has to know the other's unit.
        assert_eq!(
            Gas::new(1, ETH).to_string(),
            EVM_GAS_TO_MILLIGAS.to_string()
        );
        assert_eq!(
            Gas::new(EVM_GAS_TO_MILLIGAS, TEZ).to_string(),
            Gas::new(1, ETH).to_string()
        );
        assert_eq!(Gas::ZERO.to_string(), "0");
    }

    #[test]
    fn wire_form_round_trips() {
        for g in [Gas::ZERO, Gas::new(944, TEZ), Gas::new(30_000_000, ETH)] {
            assert_eq!(g.to_string().parse::<Gas>().unwrap(), g);
        }
    }

    #[test]
    fn wire_form_rejects_garbage() {
        assert!("".parse::<Gas>().is_err());
        assert!("12 gas in Tezos".parse::<Gas>().is_err());
        assert!("-1".parse::<Gas>().is_err());
    }

    #[test]
    fn sub_measures_in_the_right_hand_unit() {
        // A 1_000-milligas budget lends the callee ceil(1_000 / 22) = 46 EVM
        // gas. A callee reporting 45 remaining spent exactly one, so the
        // caller owes one whole EVM gas — not the 10 milligas that
        // subtracting from the un-rounded 1_000 would have billed.
        let budget = Gas::new(1_000, TEZ);
        let spent = budget - Gas::new(45, ETH);
        assert_eq!(spent, Gas::new(1, ETH));
        assert_eq!(spent.as_runtime(TEZ), EVM_GAS_TO_MILLIGAS);
    }

    #[test]
    fn sub_bills_the_rounding_loan_back() {
        // The callee burns all 46 gas it was lent. The bill is 1_012
        // milligas against the 1_000 the caller held, so the caller runs
        // out of gas rather than getting the 12-milligas loan for free.
        let budget = Gas::new(1_000, TEZ);
        let spent = budget - Gas::new(0, ETH);
        assert_eq!(spent.as_runtime(TEZ), 46 * EVM_GAS_TO_MILLIGAS);
        assert!(spent > budget);
    }

    #[test]
    fn sub_is_zero_when_the_callee_spent_nothing() {
        let budget = Gas::new(45 * EVM_GAS_TO_MILLIGAS, TEZ);
        assert_eq!(budget - Gas::new(45, ETH), Gas::new(0, ETH));
    }

    // A callee reporting more remaining than it was lent violates an
    // invariant that `debug_assert!` catches in any test build, so the
    // clamp itself is only observable in release. It bills the whole
    // budget rather than nothing; see `Sub`.
}
