// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use std::{
    borrow::{Borrow, BorrowMut},
    cell::Cell,
    collections::BTreeMap,
    marker::PhantomData,
    ops::Bound,
    rc::Rc,
};

use crate::extensions::WithGas;
use crate::keyspace::StorageV1KeySpaceCompat;
use tezos_evm_logging::{tracing::instrument, Level};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
use tezos_smart_rollup_host::{
    dal_parameters::RollupDalParameters,
    input::Message,
    metadata::RollupMetadata,
    path::{Path, RefPath},
    reveal::HostReveal,
    runtime::{RuntimeError, ValueType},
    storage::{CoreStorage, StorageV1},
    wasm::WasmHost,
};
use tezos_smart_rollup_keyspace::{KeySpaceLoader, KeySpaceLoaderError, Name};
use tezos_smart_rollup_mock::MockHost;

// Set by the node, contains the verbosity for the logs
pub const VERBOSITY_PATH: RefPath = RefPath::assert_from(b"/evm/logging_verbosity");

pub trait IsEvmNode {
    fn is_evm_node(&self) -> bool;
}

// This type has two interesting parts:
// 1. Host: BorrowMut<R> + Borrow<R>
//
//    This allows building a KernelHost that can own its host (see
//    KernelHost::default()) as long as this host can be borrowed. It makes it
//    compatible with type `KernelHost<&mut Host, _>`, which is the type built
//    in the kernel as the entrypoint only gives a mutable reference to the
//    host. As such, the individual trait implementations work for both.
//
// 2. _pd: PhantomData<R>
//
//    The host runtime type cannot be used directly as parameter of Borrow and
//    BorrowMut as it is a trait, it needs to be a parameter itself of the type.
//    However it is never used in the type itself, which will be rejected by the
//    compiler. PhantomData associates `R` to the struct with no cost at
//    runtime.
pub struct KernelHost<R, Host: BorrowMut<R> + Borrow<R>> {
    pub host: Host,
    pub execution_gas_used: u64,
    pub is_evm_node: bool,
    pub _pd: PhantomData<R>,
    loaded_keyspaces: BTreeMap<Name, Rc<Cell<bool>>>,
}

impl<R: HostReveal, Host: BorrowMut<R> + Borrow<R>> HostReveal for KernelHost<R, Host> {
    #[inline(always)]
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.borrow().reveal_preimage(hash, destination)
    }

    #[inline(always)]
    fn reveal_metadata(&self) -> RollupMetadata {
        self.host.borrow().reveal_metadata()
    }

    #[inline(always)]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.borrow().reveal_dal_page(
            published_level,
            slot_index,
            page_index,
            destination,
        )
    }

    #[inline(always)]
    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        self.host.borrow().reveal_dal_parameters()
    }
}

impl<R: StorageV1, Host: BorrowMut<R> + Borrow<R>> StorageV1 for KernelHost<R, Host> {
    #[instrument(skip(self), fields(res))]
    #[inline(always)]
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let res = self.host.borrow().store_has(path)?;

        #[cfg(feature = "tracing")]
        tracing::Span::current().record("res", res.is_some());

        Ok(res)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.borrow().store_read(path, from_offset, max_bytes)
    }

    #[instrument(skip(self, buffer, from_offset), fields(size))]
    #[inline(always)]
    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        #[cfg(feature = "tracing")]
        tracing::Span::current().record("size", buffer.len());

        self.host
            .borrow()
            .store_read_slice(path, from_offset, buffer)
    }

    #[instrument(skip(self), fields(size), err)]
    #[inline(always)]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        let res = self.host.borrow().store_read_all(path)?;

        #[cfg(feature = "tracing")]
        tracing::Span::current().record("size", res.len());

        Ok(res)
    }

    #[instrument(skip(self, src), fields(size))]
    #[inline(always)]
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        #[cfg(feature = "tracing")]
        tracing::Span::current().record("size", src.len());

        self.host.borrow_mut().store_write(path, src, at_offset)
    }

    #[instrument(skip(self, src), fields(size))]
    #[inline(always)]
    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        #[cfg(feature = "tracing")]
        tracing::Span::current().record("size", src.len());

        self.host.borrow_mut().store_write_all(path, src)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_delete(path)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_delete_value(path)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        self.host.borrow().store_count_subkeys(prefix)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_move(from_path, to_path)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_copy(from_path, to_path)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        self.host.borrow().store_value_size(path)
    }

    #[instrument(skip(self))]
    #[inline(always)]
    fn store_get_hash(
        &self,
        path: &impl Path,
    ) -> Result<[u8; tezos_smart_rollup_core::STORE_HASH_SIZE], RuntimeError> {
        self.host.borrow().store_get_hash(path)
    }
}

impl<R: WasmHost, Host: BorrowMut<R> + Borrow<R>> WasmHost for KernelHost<R, Host> {
    #[inline(always)]
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.borrow_mut().write_output(from)
    }
    #[inline(always)]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.host.borrow_mut().read_input()
    }

    #[inline(always)]
    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.borrow_mut().mark_for_reboot()
    }

    #[inline(always)]
    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }

    #[inline(always)]
    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }

    #[inline(always)]
    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }

    #[inline(always)]
    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.host.borrow().reboot_left()
    }

    #[inline(always)]
    fn runtime_version(&self) -> Result<String, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }
}

pub fn read_logs_verbosity(host: &impl StorageV1) -> Level {
    match host.store_read(&VERBOSITY_PATH, 0, 1) {
        Ok(value) if value.len() == 1 => {
            Level::try_from(value[0]).unwrap_or(Level::default())
        }
        _ => Level::default(),
    }
}

// If the flag is set, the kernel consider that this is local evm node execution.
const EVM_NODE_FLAG: RefPath = RefPath::assert_from(b"/base/__evm_node");
// TODO: L2-1158 — remove legacy fallback once all kernels are past V51
const LEGACY_EVM_NODE_FLAG: RefPath = RefPath::assert_from(b"/__evm_node");

pub fn evm_node_flag(host: &impl StorageV1) -> bool {
    Ok(Some(ValueType::Value)) == host.store_has(&EVM_NODE_FLAG)
        || Ok(Some(ValueType::Value)) == host.store_has(&LEGACY_EVM_NODE_FLAG)
}

impl<R, Host: BorrowMut<R> + Borrow<R>> WithGas for KernelHost<R, Host> {
    fn add_execution_gas(&mut self, gas: u64) {
        self.execution_gas_used += gas;
    }

    fn executed_gas(&self) -> u64 {
        self.execution_gas_used
    }
}

impl<R, Host: BorrowMut<R> + Borrow<R>> IsEvmNode for KernelHost<R, Host> {
    fn is_evm_node(&self) -> bool {
        self.is_evm_node
    }
}

impl<R: StorageV1, Host: BorrowMut<R> + Borrow<R>> KernelHost<R, Host> {
    pub fn init(host: Host) -> Self {
        let logs_verbosity = read_logs_verbosity(host.borrow());
        tezos_evm_logging::set_global_verbosity(logs_verbosity);

        let is_evm_node = evm_node_flag(host.borrow());
        Self {
            host,
            execution_gas_used: 0,
            is_evm_node,
            _pd: PhantomData,
            loaded_keyspaces: BTreeMap::new(),
        }
    }
}
pub type MockKernelHost = KernelHost<MockHost, MockHost>;

impl Default for MockKernelHost {
    fn default() -> Self {
        Self {
            host: MockHost::default(),
            execution_gas_used: 0,
            is_evm_node: false,
            _pd: PhantomData,
            loaded_keyspaces: BTreeMap::new(),
        }
    }
}

impl MockKernelHost {
    pub fn with_address(address: SmartRollupAddress) -> Self {
        let host = MockHost::with_address(&address);
        KernelHost {
            host,
            execution_gas_used: 0,
            is_evm_node: false,
            _pd: PhantomData,
            loaded_keyspaces: BTreeMap::new(),
        }
    }
}

impl<R: StorageV1 + CoreStorage, Host: BorrowMut<R> + Borrow<R>> KeySpaceLoader
    for KernelHost<R, Host>
{
    type KeySpace = StorageV1KeySpaceCompat<R::Storage>;

    fn load_or_create(
        &mut self,
        name: Name,
    ) -> Result<Self::KeySpace, KeySpaceLoaderError> {
        // Check for exact duplicate or re-load a dropped keyspace.
        if let Some(guard) = self.loaded_keyspaces.get(&name) {
            if guard.get() {
                return Err(KeySpaceLoaderError::AlreadyLoaded);
            }
            // Keyspace was previously loaded and dropped — re-load
            // immediately. Overlap checks are unnecessary: the name was
            // already validated, and overlapping names are permanently
            // rejected.
            guard.set(true);
            let guard = Rc::clone(guard);
            return Ok(StorageV1KeySpaceCompat::new(
                // SAFETY: same as below — non-overlapping prefix.
                unsafe { self.host.borrow_mut().new_storage() },
                name,
                guard,
            ));
        }

        let name_str: &str = name.as_ref();

        // Check for overlapping descendants using BTreeMap::range.
        // In the sorted map, any name starting with `name/` sorts right
        // after `name` (only '-' and '.' are valid path chars below '/').
        for (existing, _) in self
            .loaded_keyspaces
            .range::<Name, _>((Bound::Excluded(&name), Bound::Unbounded))
        {
            let existing_str: &str = existing.as_ref();
            if !existing_str.starts_with(name_str) {
                break;
            }
            if existing_str.as_bytes()[name_str.len()] == b'/' {
                return Err(KeySpaceLoaderError::Overlapping);
            }
        }

        // Check for overlapping ancestors via point lookups at each '/'
        // boundary. The number of lookups is bounded by the path depth.
        for (i, &b) in name_str.as_bytes().iter().enumerate().skip(1) {
            if b == b'/' {
                if let Ok(ancestor) = name_str[..i].parse::<Name>() {
                    if self.loaded_keyspaces.contains_key(&ancestor) {
                        return Err(KeySpaceLoaderError::Overlapping);
                    }
                }
            }
        }

        let guard = self
            .loaded_keyspaces
            .entry(name.clone())
            .or_insert_with(|| Rc::new(Cell::new(false)));
        guard.set(true);
        let guard = Rc::clone(guard);

        Ok(StorageV1KeySpaceCompat::new(
            // SAFETY: each keyspace gets its own storage handle, scoped
            // to a unique Name prefix. Handles access non-overlapping
            // parts of the same underlying storage.
            unsafe { self.host.borrow_mut().new_storage() },
            name,
            guard,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_keyspace::{
        Key, KeySpace, KeySpaceLoader, KeySpaceLoaderError,
    };

    fn key(s: &[u8]) -> &Key {
        Key::from_bytes(s).unwrap()
    }

    #[test]
    fn load_or_create_returns_usable_keyspace() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/test".parse().unwrap()).unwrap();
        ks.set(key(b"/a"), b"hello").unwrap();
        assert_eq!(ks.get(key(b"/a")), Some(b"hello".to_vec()));
    }

    #[test]
    fn load_or_create_persists_after_drop() {
        let mut host = MockKernelHost::default();

        host.load_or_create("/test".parse().unwrap())
            .unwrap()
            .set(key(b"/a"), b"v1")
            .unwrap();
        // keyspace dropped here, name released

        let ks = host.load_or_create("/test".parse().unwrap()).unwrap();
        assert_eq!(ks.get(key(b"/a")), Some(b"v1".to_vec()));
    }

    #[test]
    fn load_or_create_rejects_duplicate_while_loaded() {
        let mut host = MockKernelHost::default();
        let _ks = host.load_or_create("/evm".parse().unwrap()).unwrap();

        let err = host.load_or_create("/evm".parse().unwrap());
        assert!(matches!(err, Err(KeySpaceLoaderError::AlreadyLoaded)));
    }

    #[test]
    fn load_or_create_rejects_overlapping_names() {
        let mut host = MockKernelHost::default();
        let _ks = host.load_or_create("/evm".parse().unwrap()).unwrap();

        let err = host.load_or_create("/evm/world".parse().unwrap());
        assert!(matches!(err, Err(KeySpaceLoaderError::Overlapping)));
    }

    #[test]
    fn load_or_create_overlap_is_symmetric() {
        let mut host = MockKernelHost::default();
        let _ks = host.load_or_create("/evm/world".parse().unwrap()).unwrap();

        let err = host.load_or_create("/evm".parse().unwrap());
        assert!(matches!(err, Err(KeySpaceLoaderError::Overlapping)));
    }

    #[test]
    fn load_or_create_rejects_overlapping_even_after_drop() {
        let mut host = MockKernelHost::default();
        let ks = host.load_or_create("/evm".parse().unwrap()).unwrap();
        drop(ks);

        // Even though `/evm` was dropped, `/evm/world` still overlaps.
        let err = host.load_or_create("/evm/world".parse().unwrap());
        assert!(matches!(err, Err(KeySpaceLoaderError::Overlapping)));
    }

    // --- Branch coverage for range-based overlap detection ---

    // Range iterator hits entry `/evm-stuff` after `/evm`: starts with
    // `name` but next char is '-' (0x2D < '/'), not a descendant.
    // Verifies both keyspaces are usable and isolated.
    #[test]
    fn range_skips_dash_suffix() {
        let mut host = MockKernelHost::default();
        let mut ks1 = host.load_or_create("/evm".parse().unwrap()).unwrap();
        let mut ks2 = host.load_or_create("/evm-stuff".parse().unwrap()).unwrap();

        ks1.set(key(b"/a"), b"v1").unwrap();
        ks2.set(key(b"/a"), b"v2").unwrap();

        assert_eq!(ks1.get(key(b"/a")), Some(b"v1".to_vec()));
        assert_eq!(ks2.get(key(b"/a")), Some(b"v2".to_vec()));
    }

    // Same as above with '.' (0x2E < '/').
    #[test]
    fn range_skips_dot_suffix() {
        let mut host = MockKernelHost::default();
        let mut ks1 = host.load_or_create("/evm".parse().unwrap()).unwrap();
        let mut ks2 = host.load_or_create("/evm.backup".parse().unwrap()).unwrap();

        ks1.set(key(b"/a"), b"v1").unwrap();
        ks2.set(key(b"/a"), b"v2").unwrap();

        assert_eq!(ks1.get(key(b"/a")), Some(b"v1".to_vec()));
        assert_eq!(ks2.get(key(b"/a")), Some(b"v2".to_vec()));
    }

    // Ancestor point lookup: `/a/b` in map, loading `/a/b/c` must detect
    // the overlap. A range-only approach (`range(/a/b/..=/a/b/c)`) would
    // miss `/a/b` since it sorts before `/a/b/`.
    #[test]
    fn ancestor_found_by_point_lookup() {
        let mut host = MockKernelHost::default();
        let _ks = host.load_or_create("/a/b".parse().unwrap()).unwrap();

        let err = host.load_or_create("/a/b/c".parse().unwrap());
        assert!(matches!(err, Err(KeySpaceLoaderError::Overlapping)));
    }

    #[test]
    fn multiple_keyspaces_are_isolated() {
        let mut host = MockKernelHost::default();
        let mut ks1 = host.load_or_create("/ks1".parse().unwrap()).unwrap();
        let mut ks2 = host.load_or_create("/ks2".parse().unwrap()).unwrap();

        ks1.set(key(b"/a"), b"from_ks1").unwrap();
        ks2.set(key(b"/a"), b"from_ks2").unwrap();

        assert_eq!(ks1.get(key(b"/a")), Some(b"from_ks1".to_vec()));
        assert_eq!(ks2.get(key(b"/a")), Some(b"from_ks2".to_vec()));
    }

    #[test]
    fn copy_from_between_keyspaces() {
        let mut host = MockKernelHost::default();
        let mut src = host.load_or_create("/src".parse().unwrap()).unwrap();
        let mut dst = host.load_or_create("/dst".parse().unwrap()).unwrap();

        src.set(key(b"/a"), b"v1").unwrap();
        dst.copy_from(&src);

        assert_eq!(dst.get(key(b"/a")), Some(b"v1".to_vec()));
        assert_eq!(src.get(key(b"/a")), Some(b"v1".to_vec()));
    }

    #[test]
    fn move_from_between_keyspaces() {
        let mut host = MockKernelHost::default();
        let mut src = host.load_or_create("/src".parse().unwrap()).unwrap();
        let mut dst = host.load_or_create("/dst".parse().unwrap()).unwrap();

        src.set(key(b"/a"), b"v1").unwrap();
        dst.move_from(&mut src);

        assert_eq!(dst.get(key(b"/a")), Some(b"v1".to_vec()));
        assert!(!src.contains(key(b"/a")));
    }

    #[test]
    fn copy_from_after_clear() {
        let mut host = MockKernelHost::default();
        let mut src = host.load_or_create("/src".parse().unwrap()).unwrap();
        let mut dst = host.load_or_create("/dst".parse().unwrap()).unwrap();

        src.set(key(b"/a"), b"v1").unwrap();
        dst.set(key(b"/x"), b"old").unwrap();

        dst.clear();
        dst.copy_from(&src);

        assert_eq!(dst.get(key(b"/a")), Some(b"v1".to_vec()));
        assert!(!dst.contains(key(b"/x")));
    }

    #[test]
    fn move_from_after_clear() {
        let mut host = MockKernelHost::default();
        let mut src = host.load_or_create("/src".parse().unwrap()).unwrap();
        let mut dst = host.load_or_create("/dst".parse().unwrap()).unwrap();

        src.set(key(b"/a"), b"v1").unwrap();
        dst.set(key(b"/x"), b"old").unwrap();

        dst.clear();
        dst.move_from(&mut src);

        assert_eq!(dst.get(key(b"/a")), Some(b"v1".to_vec()));
        assert!(!dst.contains(key(b"/x")));
        assert!(!src.contains(key(b"/a")));
    }

    #[test]
    fn copy_from_empty_source_clears_destination() {
        let mut host = MockKernelHost::default();
        let src = host.load_or_create("/src".parse().unwrap()).unwrap();
        let mut dst = host.load_or_create("/dst".parse().unwrap()).unwrap();

        dst.set(key(b"/a"), b"v1").unwrap();
        dst.set(key(b"/b"), b"v2").unwrap();

        // src is empty (never written to), dst should be cleared.
        dst.copy_from(&src);
        assert!(!dst.contains(key(b"/a")));
        assert!(!dst.contains(key(b"/b")));
    }

    #[test]
    fn move_from_empty_source_clears_destination() {
        let mut host = MockKernelHost::default();
        let mut src = host.load_or_create("/src".parse().unwrap()).unwrap();
        let mut dst = host.load_or_create("/dst".parse().unwrap()).unwrap();

        dst.set(key(b"/a"), b"v1").unwrap();

        dst.move_from(&mut src);
        assert!(!dst.contains(key(b"/a")));
    }
}
