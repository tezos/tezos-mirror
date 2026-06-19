// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Ensure [TypeProperty] holds for a given [Type].

use super::TcError;
use crate::ast::Type;
use crate::gas::{tc_cost, Gas};

/// Type properties, as described in
/// <https://tezos.gitlab.io/michelson-reference/#types>
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum TypeProperty {
    Comparable,
    Passable,
    Storable,
    Pushable,
    Packable,
    BigMapValue,
    Duplicable,
    ViewInput,
    ViewOutput,
}

impl std::fmt::Display for TypeProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TypeProperty::Comparable => "comparable",
            TypeProperty::Passable => "passable",
            TypeProperty::Storable => "storable",
            TypeProperty::Pushable => "pushable",
            TypeProperty::Packable => "packable",
            TypeProperty::BigMapValue => "allowed big_map value",
            TypeProperty::Duplicable => "duplicable",
            TypeProperty::ViewInput => "allowed view input",
            TypeProperty::ViewOutput => "allowed view output",
        };
        write!(f, "{s}")
    }
}

impl Type {
    /// Ensure a given property `prop` holds for `self`. This function consumes
    /// gas, hence a mutable reference to [Gas] must be provided. The function
    /// traverses the type with a heap-driven worklist (not the Rust call
    /// stack), so adversarial deeply nested types do not overflow the WASM
    /// kernel's call stack. Worst-case complexity is O(n).
    ///
    /// If a property doesn't hold, returns [TcError::InvalidTypeProperty] for
    /// the offending sub-type (not necessarily the root). Can run out of gas,
    /// in which case it will return [TcError::OutOfGas].
    pub fn ensure_prop(&self, gas: &mut Gas, prop: TypeProperty) -> Result<(), TcError> {
        use Type::*;
        let mut work: Vec<&Type> = vec![self];
        while let Some(t) = work.pop() {
            gas.consume(tc_cost::TYPE_PROP_STEP)?;
            let invalid = || Err(TcError::InvalidTypeProperty(prop, t.clone()));
            match t {
                Nat | Int | Bool | Mutez | String | Unit | Never | Address | ChainId
                | Bytes | Key | Signature | KeyHash | Timestamp => (),
                Ticket(_) => match prop {
                    TypeProperty::Comparable
                    | TypeProperty::Pushable
                    | TypeProperty::Duplicable
                    | TypeProperty::Packable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => return invalid(),
                    TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::BigMapValue => (),
                },
                #[cfg(feature = "bls")]
                Bls12381Fr | Bls12381G1 | Bls12381G2 => match prop {
                    TypeProperty::Comparable => return invalid(),
                    TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::Packable
                    | TypeProperty::BigMapValue
                    | TypeProperty::Duplicable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => (),
                },
                Operation => match prop {
                    TypeProperty::Comparable
                    | TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::Packable
                    | TypeProperty::BigMapValue
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => return invalid(),
                    TypeProperty::Duplicable => (),
                },
                Pair(p) | Or(p) => {
                    // Push right then left so left pops first, matching the
                    // recursive `p.0.ensure_prop; p.1.ensure_prop` order.
                    work.push(&p.1);
                    work.push(&p.0);
                }
                Option(x) => work.push(x),
                List(x) => match prop {
                    TypeProperty::Comparable => return invalid(),
                    TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::Packable
                    | TypeProperty::BigMapValue
                    | TypeProperty::Duplicable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => work.push(x),
                },
                Set(x) => match prop {
                    TypeProperty::Comparable => return invalid(),
                    TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::Packable
                    | TypeProperty::BigMapValue
                    | TypeProperty::Duplicable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => work.push(x),
                },
                Map(p) => match prop {
                    TypeProperty::Comparable => return invalid(),
                    TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::Packable
                    | TypeProperty::BigMapValue
                    | TypeProperty::Duplicable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => work.push(&p.1),
                },
                BigMap(p) => match prop {
                    TypeProperty::Comparable
                    | TypeProperty::BigMapValue
                    | TypeProperty::Packable
                    | TypeProperty::Pushable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => return invalid(),
                    #[cfg(feature = "allow_lazy_storage_transfer")]
                    TypeProperty::Passable => work.push(&p.1),
                    #[cfg(not(feature = "allow_lazy_storage_transfer"))]
                    TypeProperty::Passable => return invalid(),
                    TypeProperty::Duplicable | TypeProperty::Storable => work.push(&p.1),
                },
                Contract(_) => match prop {
                    TypeProperty::Passable
                    | TypeProperty::Packable
                    | TypeProperty::Duplicable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => (),
                    TypeProperty::Comparable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::BigMapValue => return invalid(),
                },
                Lambda(_) => match prop {
                    TypeProperty::Comparable => return invalid(),
                    TypeProperty::Passable
                    | TypeProperty::Storable
                    | TypeProperty::Pushable
                    | TypeProperty::Packable
                    | TypeProperty::BigMapValue
                    | TypeProperty::Duplicable
                    | TypeProperty::ViewOutput
                    | TypeProperty::ViewInput => (),
                },
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gas::Gas;
    use std::thread;

    /// Regression: with the previous recursive `ensure_prop`, an adversarial
    /// type like `option (option (... 100k deep ... unit))` would overflow the
    /// WASM kernel's 1 MiB Rust stack inside the property check, defeating the
    /// purpose of MR2's iterative `parse_ty`. Run on a 1 MiB worker thread
    /// matching the kernel budget. `mem::forget` skips the still-recursive
    /// `Drop` on `Type` (converted to iterative in a follow-up MR).
    #[test]
    fn ensure_prop_does_not_overflow_on_deep_option_chain() {
        const DEPTH: usize = 100_000;
        thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut t = Type::Unit;
                for _ in 0..DEPTH {
                    t = Type::new_option(t);
                }
                let mut gas = Gas::default();
                let res = t.ensure_prop(&mut gas, TypeProperty::Comparable);
                // Avoid the recursive Drop on scope exit (gated on MR4).
                std::mem::forget(t);
                res.expect("deep option chain must be Comparable");
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Companion: same depth but rejected — the error must surface the
    /// offending sub-type (List), not the root, and must not overflow on
    /// the walk leading to it.
    #[test]
    fn ensure_prop_rejects_deep_set_of_list_via_iterative_walk() {
        const DEPTH: usize = 100_000;
        thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut t = Type::new_list(Type::Unit);
                for _ in 0..DEPTH {
                    t = Type::new_option(t);
                }
                let mut gas = Gas::default();
                let res = t.ensure_prop(&mut gas, TypeProperty::Comparable);
                std::mem::forget(t);
                match res {
                    Err(TcError::InvalidTypeProperty(
                        TypeProperty::Comparable,
                        offender,
                    )) => {
                        std::mem::forget(offender);
                    }
                    other => panic!("expected Comparable rejection, got {other:?}"),
                }
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }
}
