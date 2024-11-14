use incrementalmerkletree::{bridgetree, Altitude, Frontier, Hashable};
use std::mem::size_of_val;
use std::ptr;

use orchard::{bundle::Authorized, tree::MerkleHashOrchard};
use tracing::error;
use zcash_primitives::{
    merkle_tree::{
        incremental::{read_frontier_v1, write_frontier_v1},
        CommitmentTree,
    },
    transaction::components::Amount,
};

use crate::streams_ffi::{CppStreamReader, CppStreamWriter, ReadCb, StreamObj, WriteCb};

pub const MERKLE_DEPTH: u8 = 32;

//
// Operations on Merkle frontiers.
//

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_empty(
) -> *mut bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH> {
    let empty_tree = bridgetree::Frontier::<MerkleHashOrchard, MERKLE_DEPTH>::empty();
    Box::into_raw(Box::new(empty_tree))
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_clone(
    tree: *const bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
) -> *mut bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH> {
    unsafe { tree.as_ref() }
        .map(|tree| Box::into_raw(Box::new(tree.clone())))
        .unwrap_or(std::ptr::null_mut())
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_free(
    tree: *mut bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
) {
    if !tree.is_null() {
        drop(unsafe { Box::from_raw(tree) });
    }
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_parse(
    stream: Option<StreamObj>,
    read_cb: Option<ReadCb>,
) -> *mut bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH> {
    let reader = CppStreamReader::from_raw_parts(stream, read_cb.unwrap());

    match read_frontier_v1(reader) {
        Ok(parsed) => Box::into_raw(Box::new(parsed)),
        Err(e) => {
            error!("Failed to parse Orchard bundle: {}", e);
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_serialize(
    frontier: *const bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
    stream: Option<StreamObj>,
    write_cb: Option<WriteCb>,
) -> bool {
    let frontier = unsafe {
        frontier
            .as_ref()
            .expect("Orchard note commitment tree pointer may not be null.")
    };

    let writer = CppStreamWriter::from_raw_parts(stream, write_cb.unwrap());
    match write_frontier_v1(writer, frontier) {
        Ok(()) => true,
        Err(e) => {
            error!("{}", e);
            false
        }
    }
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_serialize_legacy(
    frontier: *const bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
    stream: Option<StreamObj>,
    write_cb: Option<WriteCb>,
) -> bool {
    let frontier = unsafe {
        frontier
            .as_ref()
            .expect("Orchard note commitment tree pointer may not be null.")
    };

    let writer = CppStreamWriter::from_raw_parts(stream, write_cb.unwrap());
    let commitment_tree = CommitmentTree::from_frontier(frontier);
    match commitment_tree.write(writer) {
        Ok(()) => true,
        Err(e) => {
            error!("{}", e);
            false
        }
    }
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_append_bundle(
    tree: *mut bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
    bundle: *const orchard::Bundle<Authorized, Amount>,
) -> bool {
    let tree = unsafe {
        tree.as_mut()
            .expect("Orchard note commitment tree pointer may not be null.")
    };
    if let Some(bundle) = unsafe { bundle.as_ref() } {
        for action in bundle.actions().iter() {
            if !tree.append(&MerkleHashOrchard::from_cmx(action.cmx())) {
                error!("Orchard note commitment tree is full.");
                return false;
            }
        }
    }

    true
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_root(
    tree: *const bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
    root_ret: *mut [u8; 32],
) {
    let tree = unsafe {
        tree.as_ref()
            .expect("Orchard note commitment tree pointer may not be null.")
    };

    let root_ret = unsafe {
        root_ret
            .as_mut()
            .expect("Cannot return to the null pointer.")
    };

    *root_ret = tree.root().to_bytes();
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_num_leaves(
    tree: *const bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
) -> u64 {
    let tree = unsafe {
        tree.as_ref()
            .expect("Orchard note commitment tree pointer may not be null.")
    };

    tree.position().map_or(0, |p| <u64>::from(p) + 1)
}

#[no_mangle]
pub extern "C" fn orchard_merkle_frontier_dynamic_mem_usage(
    tree: *const bridgetree::Frontier<MerkleHashOrchard, MERKLE_DEPTH>,
) -> usize {
    let tree = unsafe {
        tree.as_ref()
            .expect("Orchard note commitment tree pointer may not be null.")
    };

    size_of_val(tree) + tree.dynamic_memory_usage()
}

#[no_mangle]
pub extern "C" fn orchard_merkle_tree_empty_root(root_ret: *mut [u8; 32]) {
    let root_ret = unsafe {
        root_ret
            .as_mut()
            .expect("Cannot return to the null pointer.")
    };

    let altitude = Altitude::from(MERKLE_DEPTH);

    let digest = MerkleHashOrchard::empty_root(altitude).to_bytes();

    *root_ret = digest;
}
