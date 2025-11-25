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
    /// traverses the type, so worst-case complexity is O(n).
    ///
    /// If a property doesn't hold, returns [TcError::InvalidTypeProperty]. Can
    /// run out of gas, in which case it will return [TcError::OutOfGas].
    pub fn ensure_prop(&self, gas: &mut Gas, prop: TypeProperty) -> Result<(), TcError> {
        use Type::*;
        gas.consume(tc_cost::TYPE_PROP_STEP)?;
        let invalid_type_prop = || Err(TcError::InvalidTypeProperty(prop, self.clone()));
        match self {
            Nat | Int | Bool | Mutez | String | Unit | Never | Address | ChainId | Bytes | Key
            | Signature | KeyHash | Timestamp => (),
            Ticket(_) => match prop {
                TypeProperty::Comparable
                | TypeProperty::Pushable
                | TypeProperty::Duplicable
                | TypeProperty::Packable
                | TypeProperty::ViewOutput
                | TypeProperty::ViewInput => return invalid_type_prop(),
                TypeProperty::Passable | TypeProperty::Storable | TypeProperty::BigMapValue => (),
            },
            #[cfg(feature = "bls")]
            Bls12381Fr | Bls12381G1 | Bls12381G2 => match prop {
                TypeProperty::Comparable => return invalid_type_prop(),
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
                | TypeProperty::ViewInput => return invalid_type_prop(),
                TypeProperty::Duplicable => (),
            },
            Pair(p) | Or(p) => {
                p.0.ensure_prop(gas, prop)?;
                p.1.ensure_prop(gas, prop)?;
            }
            Option(x) => x.ensure_prop(gas, prop)?,
            List(x) => match prop {
                TypeProperty::Comparable => return invalid_type_prop(),
                TypeProperty::Passable
                | TypeProperty::Storable
                | TypeProperty::Pushable
                | TypeProperty::Packable
                | TypeProperty::BigMapValue
                | TypeProperty::Duplicable
                | TypeProperty::ViewOutput
                | TypeProperty::ViewInput => x.ensure_prop(gas, prop)?,
            },
            Set(x) => match prop {
                TypeProperty::Comparable => return invalid_type_prop(),
                TypeProperty::Passable
                | TypeProperty::Storable
                | TypeProperty::Pushable
                | TypeProperty::Packable
                | TypeProperty::BigMapValue
                | TypeProperty::Duplicable
                | TypeProperty::ViewOutput
                | TypeProperty::ViewInput => x.ensure_prop(gas, prop)?,
            },
            Map(p) => match prop {
                TypeProperty::Comparable => return invalid_type_prop(),
                TypeProperty::Passable
                | TypeProperty::Storable
                | TypeProperty::Pushable
                | TypeProperty::Packable
                | TypeProperty::BigMapValue
                | TypeProperty::Duplicable
                | TypeProperty::ViewOutput
                | TypeProperty::ViewInput => p.1.ensure_prop(gas, prop)?,
            },
            BigMap(p) => match prop {
                TypeProperty::Comparable
                | TypeProperty::BigMapValue
                | TypeProperty::Packable
                | TypeProperty::Pushable
                | TypeProperty::ViewOutput
                | TypeProperty::ViewInput => return invalid_type_prop(),
                #[cfg(feature = "allow_lazy_storage_transfer")]
                TypeProperty::Passable => p.1.ensure_prop(gas, prop)?,
                #[cfg(not(feature = "allow_lazy_storage_transfer"))]
                TypeProperty::Passable => return invalid_type_prop(),
                TypeProperty::Duplicable | TypeProperty::Storable => p.1.ensure_prop(gas, prop)?,
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
                | TypeProperty::BigMapValue => return invalid_type_prop(),
            },
            Lambda(_) => match prop {
                TypeProperty::Comparable => return invalid_type_prop(),
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
        Ok(())
    }
}
