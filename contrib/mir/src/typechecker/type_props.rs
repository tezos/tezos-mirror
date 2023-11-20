/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::TcError;
use crate::ast::Type;
use crate::gas::{tc_cost, Gas};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TypeProperty {
    Comparable,
    Passable,
    Storable,
    Pushable,
    Packable,
    BigMapValue,
    Duplicable,
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
        };
        write!(f, "{}", s)
    }
}

impl Type {
    pub fn ensure_prop(&self, gas: &mut Gas, prop: TypeProperty) -> Result<(), TcError> {
        use Type::*;
        gas.consume(tc_cost::TYPE_PROP_STEP)?;
        let invalid_type_prop = || Err(TcError::InvalidTypeProperty(prop, self.clone()));
        match self {
            Nat | Int | Bool | Mutez | String | Unit | Address | ChainId | Bytes | Key
            | Signature => (),
            Operation => match prop {
                TypeProperty::Comparable
                | TypeProperty::Passable
                | TypeProperty::Storable
                | TypeProperty::Pushable
                | TypeProperty::Packable
                | TypeProperty::BigMapValue => return invalid_type_prop(),
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
                | TypeProperty::Duplicable => x.ensure_prop(gas, prop)?,
            },
            Map(p) => match prop {
                TypeProperty::Comparable => return invalid_type_prop(),
                TypeProperty::Passable
                | TypeProperty::Storable
                | TypeProperty::Pushable
                | TypeProperty::Packable
                | TypeProperty::BigMapValue
                | TypeProperty::Duplicable => p.1.ensure_prop(gas, prop)?,
            },
            Contract(_) => match prop {
                TypeProperty::Passable | TypeProperty::Packable | TypeProperty::Duplicable => (),
                TypeProperty::Comparable
                | TypeProperty::Storable
                | TypeProperty::Pushable
                | TypeProperty::BigMapValue => return invalid_type_prop(),
            },
        }
        Ok(())
    }
}
