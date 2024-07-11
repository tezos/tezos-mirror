// SPDX-FileCopyrightText: 2023-2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::pvm::{
    dummy_pvm::{DummyPvm, PvmStorage, PvmStorageError},
    PvmHooks, PvmStatus,
};
use crate::storage::{self, StorageError};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use ocaml::{Pointer, ToValue};
use sha2::{Digest, Sha256};
use std::{fs, str};

const HERMIT_LOADER: &[u8] = include_bytes!("../../assets/hermit-loader");

#[ocaml::sig]
pub struct Repo(PvmStorage);

#[ocaml::sig]
pub struct State(DummyPvm);

#[ocaml::sig]
pub struct Id(storage::Hash);

#[ocaml::sig]
pub struct Hooks(PvmHooks<'static>);

ocaml::custom!(Repo);
ocaml::custom!(State);
ocaml::custom!(Id);
ocaml::custom!(Hooks);

#[derive(ocaml::FromValue, ocaml::ToValue, IntoPrimitive, TryFromPrimitive)]
#[ocaml::sig("Evaluating | WaitingForInput | WaitingForMetadata")]
#[repr(u8)]
pub enum Status {
    Evaluating,
    WaitingForInput,
    WaitingForMetadata,
}

impl From<PvmStatus> for Status {
    fn from(item: PvmStatus) -> Self {
        Status::try_from(item as u8).expect("Invalid conversion")
    }
}

impl From<Status> for PvmStatus {
    fn from(item: Status) -> Self {
        PvmStatus::try_from(item as u8).expect("Invalid conversion")
    }
}

#[ocaml::func]
#[ocaml::sig("unit -> hooks")]
pub fn octez_riscv_default_pvm_hooks() -> Pointer<Hooks> {
    Hooks(PvmHooks::default()).into()
}

#[ocaml::func]
#[ocaml::sig("bytes -> id")]
pub fn octez_riscv_id_unsafe_of_raw_bytes(s: &[u8]) -> Pointer<Id> {
    assert!(s.len() == storage::DIGEST_SIZE);
    let hash: storage::Hash = s.try_into().unwrap();
    Id(hash).into()
}

#[ocaml::func]
#[ocaml::sig("id -> bytes")]
pub fn octez_riscv_storage_id_to_raw_bytes(id: Pointer<Id>) -> [u8; 32] {
    id.as_ref().0
}

#[ocaml::func]
#[ocaml::sig("id -> id -> bool")]
pub fn octez_riscv_storage_id_equal(id1: Pointer<Id>, id2: Pointer<Id>) -> bool {
    id1.as_ref().0 == id2.as_ref().0
}

#[ocaml::func]
#[ocaml::sig("state -> state -> bool")]
pub fn octez_riscv_storage_state_equal(state1: Pointer<State>, state2: Pointer<State>) -> bool {
    state1.as_ref().0.to_bytes() == state2.as_ref().0.to_bytes()
}

#[ocaml::func]
#[ocaml::sig("unit -> state")]
pub fn octez_riscv_storage_state_empty() -> Pointer<State> {
    State(DummyPvm::empty()).into()
}

#[ocaml::func]
#[ocaml::sig("string -> repo")]
pub fn octez_riscv_storage_load(path: String) -> Result<Pointer<Repo>, ocaml::Error> {
    match PvmStorage::load(path) {
        Ok(repo) => Ok(Repo(repo).into()),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("repo -> unit")]
pub fn octez_riscv_storage_close(_repo: Pointer<Repo>) {}

#[ocaml::func]
#[ocaml::sig("repo -> state -> id")]
pub fn octez_riscv_storage_commit(
    mut repo: Pointer<Repo>,
    state: Pointer<State>,
) -> Result<Pointer<Id>, ocaml::Error> {
    let state = &state.as_ref().0;
    match repo.as_mut().0.commit(state) {
        Ok(hash) => Ok(Id(hash).into()),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> state option")]
pub fn octez_riscv_storage_checkout(
    repo: Pointer<Repo>,
    id: Pointer<Id>,
) -> Result<Option<Pointer<State>>, ocaml::Error> {
    let id = &id.as_ref().0;
    match repo.as_ref().0.checkout(id) {
        Ok(state) => Ok(Some(State(state).into())),
        Err(PvmStorageError::StorageError(StorageError::NotFound(_))) => Ok(None),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("state -> status")]
pub fn octez_riscv_get_status(state: Pointer<State>) -> Status {
    state.as_ref().0.get_status().into()
}

#[ocaml::func]
#[ocaml::sig("status -> string")]
pub fn octez_riscv_string_of_status(status: Status) -> String {
    PvmStatus::from(status).to_string()
}

#[ocaml::func]
#[ocaml::sig("state -> hooks -> state")]
pub fn octez_riscv_compute_step(
    state: Pointer<State>,
    mut hooks: Pointer<Hooks>,
) -> Pointer<State> {
    State(state.as_ref().0.compute_step(&mut hooks.as_mut().0)).into()
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> hooks -> (state * int64)")]
pub fn octez_riscv_compute_step_many(
    max_steps: usize,
    state: Pointer<State>,
    mut hooks: Pointer<Hooks>,
) -> (Pointer<State>, i64) {
    let (s, steps) = state
        .as_ref()
        .0
        .compute_step_many(&mut hooks.as_mut().0, max_steps);
    (State(s).into(), steps)
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_tick(state: Pointer<State>) -> u64 {
    state.as_ref().0.get_tick()
}

#[ocaml::func]
#[ocaml::sig("state -> int32 option")]
pub fn octez_riscv_get_level(state: Pointer<State>) -> Option<u32> {
    state.as_ref().0.get_current_level()
}

fn verify_checksum(contents: &[u8], checksum: &str) -> bool {
    let mut hasher = Sha256::new();
    hasher.update(&contents);
    let digest = format!("{:x}", hasher.finalize());
    digest == checksum
}

fn read_boot_sector_binary(path: &str, checksum: &str) -> Vec<u8> {
    let binary = fs::read(path).unwrap_or_else(|_| {
        panic!("Error installing boot sector: could not read binary at {path}")
    });
    assert!(
        verify_checksum(&binary, checksum),
        "Error installing boot sector: checksum mismatch for {path}"
    );
    binary
}

#[ocaml::func]
#[ocaml::sig("state -> bytes -> state")]
pub fn octez_riscv_install_boot_sector(
    state: Pointer<State>,
    boot_sector: &[u8],
) -> Pointer<State> {
    // RISC-V kernels are too large to be originated directly. In order to
    // temporarily bypass this limitation (TODO: RV-109 Port kernel installer to RISC-V)
    // the boot sector is installed by loading it from a path passed at origination
    // after checking consistency with the provided checksum. Optionally, the Hermit loader
    // can also be passed in the origination string:
    // "kernel:<path to kernel>:<kernel checksum>[:<path to loader>:<loader checksum>]"
    // Any string not matching this format will be treated as an actual kernel to be installed.
    let state = &state.as_ref().0;
    match str::from_utf8(boot_sector) {
        Ok(s) => {
            let parts: Vec<&str> = s.split(':').collect();
            match parts.as_slice() {
                ["kernel", kernel_path, kernel_checksum] => {
                    let kernel = read_boot_sector_binary(kernel_path, kernel_checksum);
                    return State(state.install_boot_sector(&HERMIT_LOADER, &kernel)).into();
                }
                ["kernel", kernel_path, kernel_checksum, loader_path, loader_checksum] => {
                    let kernel = read_boot_sector_binary(kernel_path, kernel_checksum);
                    let loader = read_boot_sector_binary(loader_path, loader_checksum);
                    return State(state.install_boot_sector(&loader, &kernel)).into();
                }
                _ => (),
            }
        }
        Err(_) => (),
    };
    State(state.install_boot_sector(&HERMIT_LOADER, boot_sector)).into()
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_state_hash(state: Pointer<State>) -> [u8; 32] {
    state.as_ref().0.hash()
}

#[ocaml::func]
#[ocaml::sig("state -> int32 -> int64 -> bytes -> state")]
pub fn octez_riscv_set_input_message(
    state: Pointer<State>,
    level: u32,
    message_counter: u64,
    input: &[u8],
) -> Pointer<State> {
    State(
        state
            .as_ref()
            .0
            .set_input_message(level, message_counter, input.to_vec()),
    )
    .into()
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_message_counter(state: Pointer<State>) -> u64 {
    state.as_ref().0.get_message_counter()
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> string -> (unit, [`Msg of string]) result")]
pub unsafe fn octez_riscv_storage_export_snapshot(
    repo: Pointer<Repo>,
    id: Pointer<Id>,
    path: &str,
) -> Result<(), ocaml::Value> {
    let id = &id.as_ref().0;
    repo.as_ref().0.export_snapshot(id, path).map_err(|e| {
        let s = format!("{e:?}");
        ocaml::Value::hash_variant(gc, "Msg", Some(s.to_value(gc)))
    })
}
