// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    bindings::{self, BindingsError},
    constants::REBOOT_FLAG,
    host::{Hasher, Host, Input, RuntimeVersion},
    reveal,
};
use log::trace;
use runtime_bifrost::internal_runtime::InternalRuntime as BifrostInternalRuntime;
use runtime_calypso::internal_runtime::InternalRuntime as CalypsoInternalRuntime;
use runtime_calypso2::internal_runtime::InternalRuntime as Calypso2InternalRuntime;
use runtime_dionysus::internal_runtime::InternalRuntime as DionysusInternalRuntime;
use runtime_dionysus_r1::internal_runtime::InternalRuntime as DionysusR1InternalRuntime;
use runtime_ebisu::internal_runtime::InternalRuntime as EbisuInternalRuntime;
use tezos_smart_rollup_core_v1::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_host_v1::{
    dal_parameters::RollupDalParameters,
    input::Message,
    metadata::RollupMetadata,
    path::{Path, RefPath},
    runtime::{Runtime, RuntimeError, ValueType},
    Error as HostError,
};

const REBOOT_FLAG_PATH: RefPath = RefPath::assert_from(REBOOT_FLAG.as_bytes());

fn from_binding_error(e: BindingsError) -> RuntimeError {
    match e {
        BindingsError::HostFuncError(e) => RuntimeError::HostErr(HostError::from(e)),
        BindingsError::OCamlError(e) => {
            // Something went very wrong, we are probably in an unreachable state, which
            // deserves a panic. This will raise an exception on the OCaml side.
            panic!("OCaml bindings failed with error: {:?}", e)
        }
    }
}

impl Host {
    // Compare to its counterpart in the kernel SDK, we only check if the path exists on error.
    fn check_path_exists_v1<'a>(
        &'a self,
        path: &'a impl Path,
    ) -> impl FnOnce(RuntimeError) -> RuntimeError + 'a {
        |err| {
            if let Ok(Some(_)) = self.store_has(path) {
                err
            } else {
                RuntimeError::PathNotFound
            }
        }
    }

    // Compare to its counterpart in the kernel SDK, we only check if the path exists on error.
    fn check_path_has_value_v1<'a>(
        &'a self,
        path: &'a impl Path,
    ) -> impl FnOnce(RuntimeError) -> RuntimeError + 'a {
        |err| {
            if let Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) = self.store_has(path) {
                err
            } else {
                RuntimeError::PathNotFound
            }
        }
    }
}

impl Runtime for Host {
    fn write_output(&mut self, _from: &[u8]) -> Result<(), RuntimeError> {
        trace!("write_output()");
        // The outbox is only useful in the context of the rollup node, in order to enable
        // withdrawal. On the EVM node side, we can just turn it into a no-op.
        Ok(())
    }

    fn write_debug(&self, msg: &str) {
        self.write_debug(msg.as_bytes())
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        trace!("read_input()");

        match self.next_input() {
            None => Ok(None),
            Some(Input {
                level,
                index,
                payload,
            }) => Ok(Some(Message::new(level, index, payload))),
        }
    }

    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        trace!("store_has({path})");
        let res = bindings::store_has(&self.tree(), path.as_bytes());

        match res.map_err(from_binding_error)? {
            0 => Ok(None),
            1 => Ok(Some(ValueType::Value)),
            2 => Ok(Some(ValueType::Subtree)),
            3 => Ok(Some(ValueType::ValueWithSubtree)),
            _ => {
                // `bindings::store_has` should never return a value that is not between 0 and 3.
                unreachable!()
            }
        }
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_read({path})");
        let max_bytes = std::cmp::min(max_bytes, MAX_FILE_CHUNK_SIZE);

        let res = bindings::store_read(&self.tree(), path.as_bytes(), from_offset, max_bytes)
            .map_err(from_binding_error)
            .map_err(self.check_path_has_value_v1(path))?;

        Ok(res.as_bytes().to_owned())
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        trace!("store_read_slice({path})");
        let max_bytes = std::cmp::min(buffer.len(), MAX_FILE_CHUNK_SIZE);
        let res = bindings::store_read(&self.tree(), path.as_bytes(), from_offset, max_bytes)
            .map_err(from_binding_error)?;

        buffer.copy_from_slice(res.as_bytes());

        Ok(res.as_bytes().len())
    }

    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_read_all({path})");
        let res = bindings::read_value(&self.tree(), path.as_bytes())
            .map_err(from_binding_error)
            .map_err(self.check_path_exists_v1(path))?;
        Ok(res.as_bytes().to_owned())
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        trace!("store_write({path})");
        let (new_tree, _size) =
            bindings::store_write(&self.tree(), path.as_bytes(), at_offset, src)
                .map_err(from_binding_error)?;
        self.set_tree(new_tree);

        Ok(())
    }

    fn store_write_all<T: Path>(&mut self, path: &T, src: &[u8]) -> Result<(), RuntimeError> {
        trace!("store_write_all({path})");
        let new_tree = bindings::store_write_all(&self.tree(), path.as_bytes(), src)
            .map_err(from_binding_error)?;
        self.set_tree(new_tree);

        Ok(())
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        trace!("store_delete({path})");
        if let Ok(None) = Runtime::store_has(self, path) {
            return Err(RuntimeError::PathNotFound);
        }

        let new_tree = bindings::store_delete(&self.tree(), path.as_bytes(), false)
            .map_err(from_binding_error)?;
        self.set_tree(new_tree);

        Ok(())
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        trace!("store_delete_value({path})");
        let new_tree = bindings::store_delete(&self.tree(), path.as_bytes(), true)
            .map_err(from_binding_error)?;
        self.set_tree(new_tree);

        Ok(())
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        trace!("store_count_subkeys({prefix})");
        let res = bindings::store_list_size(&self.tree(), prefix.as_bytes())
            .map_err(from_binding_error)?;

        Ok(res as u64)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        trace!("store_move({from_path}, {to_path})");
        let new_tree = bindings::store_move(&self.tree(), from_path.as_bytes(), to_path.as_bytes())
            .map_err(from_binding_error)
            .map_err(self.check_path_exists_v1(from_path))?;
        self.set_tree(new_tree);

        Ok(())
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        trace!("store_copy({from_path}, {to_path})");
        let new_tree = bindings::store_copy(&self.tree(), from_path.as_bytes(), to_path.as_bytes())
            .map_err(from_binding_error)
            .map_err(self.check_path_exists_v1(from_path))?;
        self.set_tree(new_tree);

        Ok(())
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; 33],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        trace!("reveal_preimage({})", hex::encode(hash));

        match reveal::preimage(
            hash.as_slice(),
            self.preimages_dir.as_str(),
            self.preimages_endpoint
                .as_ref()
                .map(|endpoint| endpoint.as_str()),
        ) {
            Ok(buf) => {
                let to_copy = std::cmp::min(destination.len(), buf.len());
                let destination = &mut destination[..to_copy];
                destination.copy_from_slice(&buf[..to_copy]);
                Ok(buf.len())
            }
            Err(reveal_err) => {
                panic!(
                    "Could not reveal preimage for {}: {}",
                    hex::encode(hash),
                    reveal_err
                )
            }
        }
    }

    fn reveal_dal_page(
        &self,
        _published_level: i32,
        _slot_index: u8,
        _page_index: i16,
        _destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        // Only the kernels executed by a Rollup Node will ever use this function.
        unimplemented!()
    }

    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        // Only the kernels executed by a Rollup Node will ever use this function.
        unimplemented!()
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        trace!("store_value_size({path})");
        match self.version {
            RuntimeVersion::V0 => {
                let res = bindings::store_value_size(&self.tree(), path.as_bytes())
                    .map_err(from_binding_error)
                    .map_err(self.check_path_exists_v1(path))?;

                Ok(res
                    .try_into()
                    .expect("inconsistent result from bindings::store_value_size"))
            }
            RuntimeVersion::V1 => {
                // Changes compared to V0 introduced by
                // https://gitlab.com/tezos/tezos/-/merge_requests/15897.
                let res = bindings::store_value_size(&self.tree(), path.as_bytes())
                    .map_err(from_binding_error)
                    .map_err(self.check_path_has_value_v1(path))?;

                Ok(res
                    .try_into()
                    .expect("inconsistent result from bindings::store_value_size"))
            }
        }
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        trace!("mark_for_reboot()");
        self.store_write_all(&REBOOT_FLAG_PATH, b"")
    }

    fn reveal_metadata(&self) -> RollupMetadata {
        trace!("reveal_metadata()");
        RollupMetadata {
            // Origination level is not used, so we give a default value
            origination_level: 0,
            raw_rollup_address: self.rollup_address().as_bytes().clone(),
        }
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        // This function is never used, and the underlying logic is not implemented.
        unimplemented!()
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        // This function is never used, and the underlying logic is not implemented.
        unimplemented!()
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        // This function is never used, and the underlying logic is not implemented.
        unimplemented!()
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        trace!("reboot_left()");
        // We don’t implement the reboot counter logic, so the value in the tree will always be the
        // same.
        Ok(1001)
    }

    fn runtime_version(&self) -> Result<String, RuntimeError> {
        // This function is never used
        unimplemented!()
    }
}

impl BifrostInternalRuntime for Host {
    fn __internal_store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_get_hash({path})");
        let hash =
            bindings::store_get_hash(&self.tree(), path.as_bytes()).map_err(from_binding_error)?;

        Ok(hash.as_bytes().to_vec())
    }
}

impl CalypsoInternalRuntime for Host {
    fn __internal_store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_get_hash({path})");
        let hash =
            bindings::store_get_hash(&self.tree(), path.as_bytes()).map_err(from_binding_error)?;

        Ok(hash.as_bytes().to_vec())
    }
}

impl Calypso2InternalRuntime for Host {
    fn __internal_store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_get_hash({path})");
        let hash =
            bindings::store_get_hash(&self.tree(), path.as_bytes()).map_err(from_binding_error)?;

        Ok(hash.as_bytes().to_vec())
    }
}

impl DionysusInternalRuntime for Host {
    fn __internal_store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_get_hash({path})");
        let hash =
            bindings::store_get_hash(&self.tree(), path.as_bytes()).map_err(from_binding_error)?;

        Ok(hash.as_bytes().to_vec())
    }
}

impl DionysusR1InternalRuntime for Host {
    fn __internal_store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_get_hash({path})");
        let hash =
            bindings::store_get_hash(&self.tree(), path.as_bytes()).map_err(from_binding_error)?;

        Ok(hash.as_bytes().to_vec())
    }
}

impl EbisuInternalRuntime for Hasher {
    fn __internal_store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        trace!("store_get_hash({path})");
        let hash = bindings::store_get_hash(&self.0.borrow(), path.as_bytes())
            .map_err(from_binding_error)?;

        Ok(hash.as_bytes().to_vec())
    }
}
