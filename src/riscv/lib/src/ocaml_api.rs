// SPDX-FileCopyrightText: 2023-2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::pvm::dummy_pvm::DummyPvm;
use crate::storage::{self, StorageError};
use ocaml::{Pointer, ToValue};

#[ocaml::sig]
pub struct Repo(storage::Repo<DummyPvm>);

#[ocaml::sig]
pub struct State(DummyPvm);

#[ocaml::sig]
pub struct Id(storage::Hash);

#[ocaml::sig]
pub struct Status(String);

ocaml::custom!(Repo);
ocaml::custom!(State);
ocaml::custom!(Id);
ocaml::custom!(Status);

#[ocaml::func]
#[ocaml::sig("string -> id")]
pub fn octez_riscv_id_unsafe_of_raw_string(s: String) -> Pointer<Id> {
    assert!(s.len() == storage::DIGEST_SIZE);
    let hash: storage::Hash = s.as_bytes().try_into().unwrap();
    Id(hash).into()
}

#[ocaml::func]
#[ocaml::sig("id -> string")]
pub fn octez_riscv_storage_id_to_raw_string(id: Pointer<Id>) -> String {
    std::str::from_utf8(&id.as_ref().0).unwrap().to_string()
}

#[ocaml::func]
#[ocaml::sig("id -> id -> bool")]
pub fn octez_riscv_storage_id_equal(id1: Pointer<Id>, id2: Pointer<Id>) -> bool {
    id1.as_ref().0 == id2.as_ref().0
}

#[ocaml::func]
#[ocaml::sig("state -> state -> bool")]
pub fn octez_riscv_storage_state_equal(state1: Pointer<State>, state2: Pointer<State>) -> bool {
    state1.as_ref().0 == state2.as_ref().0
}

#[ocaml::func]
#[ocaml::sig("unit -> state")]
pub fn octez_riscv_storage_state_empty() -> Pointer<State> {
    State(DummyPvm::empty()).into()
}

#[ocaml::func]
#[ocaml::sig("string -> repo")]
pub fn octez_riscv_storage_load(path: String) -> Result<Pointer<Repo>, ocaml::Error> {
    match storage::Repo::load(path) {
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
        Err(StorageError::NotFound(_)) => Ok(None),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("state -> status")]
pub fn octez_riscv_get_status(_state: Pointer<State>) -> Pointer<Status> {
    Status("dummy_value".to_string()).into()
}

#[ocaml::func]
#[ocaml::sig("status -> string")]
pub fn octez_riscv_string_of_status(_status: Pointer<Status>) -> String {
    unimplemented!()
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_compute_step(_state: Pointer<State>) -> Pointer<State> {
    unimplemented!()
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (state * int64)")]
pub fn octez_riscv_compute_step_many(
    _max_steps: usize,
    _state: Pointer<State>,
) -> (Pointer<State>, i64) {
    unimplemented!()
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_tick(_state: Pointer<State>) -> i64 {
    unimplemented!()
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
