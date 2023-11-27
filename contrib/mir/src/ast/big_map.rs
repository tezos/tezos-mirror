/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use num_bigint::BigInt;
use std::{collections::BTreeMap, fmt::Display};
use typed_arena::Arena;

use super::{Micheline, Type, TypedValue};

/// Id of big map in the lazy storage.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BigMapId(BigInt);

impl Display for BigMapId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Represents a big_map value.
///
/// Big map is split into two parts - one is in the lazy storage, and another is
/// an in-memory overlay that carries a diff from the map in the storage.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BigMap<'a> {
    /// Id of the big map in the lazy storage.
    ///
    /// Big map can be backed by no map in the lazy storage and yet stay fully
    /// in memory, in such case this field is `None`.
    id: Option<BigMapId>,

    /// In-memory part, carries the diff that is to be applied to the map in the
    /// storage.
    ///
    /// Normally, execution of all writing instructions update this part, and at
    /// certain key points like the end of the contract execution this diff is
    /// dumped into the storage. Change in storage can be applied in-place or,
    /// if necessary, with copy of the stored map.
    overlay: BTreeMap<TypedValue<'a>, Option<TypedValue<'a>>>,

    key_type: Type,
    value_type: Type,
}

impl<'a> BigMap<'a> {
    /// Michelson's `GET`.
    pub fn get(
        &self,
        arena: &'a Arena<Micheline<'a>>,
        key: &TypedValue,
        storage: &impl LazyStorage<'a>,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        Ok(match self.overlay.get(key) {
            // If the key is mentioned in the overlay, the associated value is
            // always used, even if it is `None` (and `get` returned
            // `Some(None)`) which means removal.
            Some(change) => change.clone(),
            None => match &self.id {
                Some(id) => storage.big_map_get(arena, id, key)?,
                None => None,
            },
        })
    }

    /// Michelson's `MEM`.
    pub fn mem(
        &self,
        key: &TypedValue,
        storage: &impl LazyStorage<'a>,
    ) -> Result<bool, LazyStorageError> {
        Ok(match self.overlay.get(key) {
            // If the key is mentioned in the overlay, the associated value is
            // always used, even if it is `None` (and `get` returned
            // `Some(None)`) which means removal.
            Some(change) => change.is_some(),
            None => match &self.id {
                Some(id) => storage.big_map_mem(id, key)?,
                None => false,
            },
        })
    }

    /// Michelson's `UPDATE`.
    pub fn update(&mut self, key: TypedValue<'a>, value: Option<TypedValue<'a>>) {
        self.overlay.insert(key, value);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum LazyStorageError {
    #[error("decode failed {0}")]
    DecodingError(String),
    #[error("{0}")]
    OtherError(String),
}

/// All the operations for working with the lazy storage.
///
/// Note that in the Tezos protocol implementation, work with this layer is
/// observable. When you call a contract with `octez-client`, you can see, for
/// instance:
///
/// ```txt
/// Updated storage: (Pair 69183 70325)
/// Updated big_maps:
///   Clear map(69179)
///   New map(70325) of type (big_map int int)
///   Set map(69183)[5] to 5
/// ```
///
/// So we try to mimic what the Tezos protocol does, and do it carefully so that
/// if we also need to log actions done at this layer, it would be close to what
/// the Tezos protocol does.
///
/// Lifetime parameter `'a` matches the lifetime of the arena used to place
/// Micheline.
pub trait LazyStorage<'a> {
    /// Get a value under the given key of the given big map.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key type must match the type of key of the stored map.
    fn big_map_get(
        &self,
        arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError>;

    /// Check whether a value is present under the given key of the given big
    /// map.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key type must match the type of key of the stored map.
    fn big_map_mem(&self, id: &BigMapId, key: &TypedValue) -> Result<bool, LazyStorageError>;

    /// Add or remove a value in big map, accepts `Option` as value like in
    /// Michelson.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key and value types must match the type of key of the stored map.
    fn big_map_update(
        &mut self,
        id: &BigMapId,
        key: TypedValue<'a>,
        value: Option<TypedValue<'a>>,
    ) -> Result<(), LazyStorageError>;

    /// Update big map with multiple changes, generalizes
    /// [LazyStorage::big_map_update].
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key and value types must match the type of key of the stored map.
    fn big_map_bulk_update(
        &mut self,
        id: &BigMapId,
        entries_iter: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
    ) -> Result<(), LazyStorageError> {
        for (k, v) in entries_iter {
            self.big_map_update(id, k, v)?
        }
        Ok(())
    }

    /// Get key and value types of the map.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    fn big_map_get_type(&self, id: &BigMapId) -> Result<(&Type, &Type), LazyStorageError>;

    /// Allocate a new empty big map.
    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
    ) -> Result<BigMapId, LazyStorageError>;

    /// Allocate a new big map, filling it with the contents from another map
    /// in the lazy storage.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    fn big_map_copy(&mut self, id: &BigMapId) -> Result<BigMapId, LazyStorageError>;

    /// Remove a big map.
    ///
    /// The caller is obliged to never use this big map ID in the given
    /// storage.
    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MapInfo<'a> {
    map: BTreeMap<TypedValue<'a>, TypedValue<'a>>,
    key_type: Type,
    value_type: Type,
}

/// Simple implementation for [LazyStorage].
#[derive(Clone)]
pub struct InMemoryLazyStorage<'a> {
    next_id: BigInt,
    big_maps: BTreeMap<BigMapId, MapInfo<'a>>,
}

impl<'a> InMemoryLazyStorage<'a> {
    pub fn new() -> Self {
        InMemoryLazyStorage {
            next_id: 0.into(),
            big_maps: BTreeMap::new(),
        }
    }

    fn get_next_id(&mut self) -> BigMapId {
        let id = BigMapId(self.next_id.clone());
        self.next_id += 1;
        id
    }
}

impl<'a> Default for InMemoryLazyStorage<'a> {
    fn default() -> Self {
        InMemoryLazyStorage::new()
    }
}

impl<'a> InMemoryLazyStorage<'a> {
    fn access_big_map(&self, id: &BigMapId) -> Result<&MapInfo<'a>, LazyStorageError> {
        self.big_maps
            .get(id)
            .ok_or_else(|| panic!("Non-existent big map by id {id}"))
    }

    fn access_big_map_mut(&mut self, id: &BigMapId) -> Result<&mut MapInfo<'a>, LazyStorageError> {
        self.big_maps
            .get_mut(id)
            .ok_or_else(|| panic!("Non-existent big map by id {id}"))
    }
}

impl<'a> LazyStorage<'a> for InMemoryLazyStorage<'a> {
    fn big_map_get(
        &self,
        _arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        let info = self.access_big_map(id)?;
        Ok(info.map.get(key).cloned())
    }

    fn big_map_mem(&self, id: &BigMapId, key: &TypedValue) -> Result<bool, LazyStorageError> {
        let info = self.access_big_map(id)?;
        Ok(info.map.contains_key(key))
    }

    fn big_map_update(
        &mut self,
        id: &BigMapId,
        key: TypedValue<'a>,
        value: Option<TypedValue<'a>>,
    ) -> Result<(), LazyStorageError> {
        let info = self.access_big_map_mut(id)?;
        match value {
            None => {
                info.map.remove(&key);
            }
            Some(value) => {
                info.map.insert(key, value);
            }
        }
        Ok(())
    }

    fn big_map_get_type(&self, id: &BigMapId) -> Result<(&Type, &Type), LazyStorageError> {
        let info = self.access_big_map(id)?;
        Ok((&info.key_type, &info.value_type))
    }

    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
    ) -> Result<BigMapId, LazyStorageError> {
        let id = self.get_next_id();
        self.big_maps.insert(
            id.clone(),
            MapInfo {
                map: BTreeMap::new(),
                key_type: key_type.clone(),
                value_type: value_type.clone(),
            },
        );
        Ok(id)
    }

    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError> {
        self.big_maps.remove(id);
        Ok(())
    }

    fn big_map_copy(&mut self, copied_id: &BigMapId) -> Result<BigMapId, LazyStorageError> {
        let id = self.get_next_id();
        let info = self.access_big_map(copied_id)?.clone();
        self.big_maps.insert(id.clone(), info);
        Ok(id)
    }
}

/// This will also implement [LazyStorage].
///
/// Worth mentioning, that this type will eventually wrap `&mut impl Runtime`
/// (or rather `&mut R (where R: Runtime)` which has to be a mere reference,
/// this `Runtime` will provide access to rollup's persistent storage. And this
/// potentially indicates some problems for us.
///
/// * We will have to put this storage to the context, meaning that in `Ctx` we
///   will have to account for lifetimes and for the `R` generic argument.
/// * One cannot put `&mut impl Runtime` twice into context because of borrowing
///   restrictions. So trying to have e.g. `RollupStorage` as one context field,
///   and some `OriginatedContractParameterGetter` that also works via `Runtime`
///   as another context field won't be possible. We may end up with the entire
///   `Ctx` implementing all the traits that our engine should be polymorphic
///   over (`LazyStorage` is probably one of them), which is ugly and
///   boilerplat-y.
/// * The caller may have a similar problem - he will have to give
///   typechecker/interpreter's context exclusive access to `Runtime`. We will
///   have to make sure it doesn't restrict the caller from e.g. writing
///   something to the storage between typechecker and interpreter calls.
#[allow(dead_code)]
struct RollupStorage();

#[cfg(test)]
mod test_big_map_operations {
    use super::*;

    fn check_get_mem<'a>(
        map: &BigMap<'a>,
        arena: &'a Arena<Micheline<'a>>,
        storage: &mut impl LazyStorage<'a>,
        key: TypedValue,
        expected_val: Option<TypedValue<'a>>,
    ) {
        assert_eq!(map.get(arena, &key, storage).unwrap(), expected_val);
        assert_eq!(map.mem(&key, storage).unwrap(), expected_val.is_some());
    }

    #[test]
    fn test_get_mem_in_memory() {
        let arena = &Arena::new();
        let storage = &mut InMemoryLazyStorage::new();
        let map = BigMap {
            id: None,
            overlay: BTreeMap::from([(TypedValue::int(1), Some(TypedValue::int(1)))]),
            key_type: Type::Int,
            value_type: Type::Int,
        };

        check_get_mem(&map, arena, storage, TypedValue::int(0), None);
        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(1),
            Some(TypedValue::int(1)),
        );
    }

    #[test]
    fn test_get_mem_backed_by_storage() {
        let arena = &Arena::new();
        let storage = &mut InMemoryLazyStorage::new();
        let map_id = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(1), Some(TypedValue::int(1)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(2), Some(TypedValue::int(2)))
            .unwrap();
        let map = BigMap {
            id: Some(map_id),
            overlay: BTreeMap::from([
                (TypedValue::int(1), Some(TypedValue::int(-1))),
                (TypedValue::int(2), None),
                (TypedValue::int(3), Some(TypedValue::int(3))),
            ]),
            key_type: Type::Int,
            value_type: Type::Int,
        };

        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(0),
            Some(TypedValue::int(0)),
        );
        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(1),
            Some(TypedValue::int(-1)),
        );
        check_get_mem(&map, arena, storage, TypedValue::int(2), None);
        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(3),
            Some(TypedValue::int(3)),
        );
    }
}
