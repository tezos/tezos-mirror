// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod move_semantics;
mod pointer_apply;

use std::fs;
use std::str;

use arbitrary_int::u31;
use move_semantics::ImmutableState;
use move_semantics::MutableState;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use ocaml::Pointer;
use ocaml::ToValue;
use octez_riscv::pvm::InputRequest as PvmInputRequest;
use octez_riscv::pvm::PvmInput;
use octez_riscv::pvm::PvmStatus;
use octez_riscv::pvm::hooks::NoHooks;
use octez_riscv::pvm::hooks::PvmHooks;
use octez_riscv::pvm::hooks::StdoutDebugHooks;
use octez_riscv::pvm::node_pvm::NodePvm;
use octez_riscv::pvm::node_pvm::PvmStorage;
use octez_riscv::pvm::node_pvm::PvmStorageError;
use octez_riscv::state_backend::owned_backend::Owned;
use octez_riscv::state_backend::proof_backend::proof::MerkleProof;
use octez_riscv::state_backend::proof_backend::proof::Proof as PvmProof;
use octez_riscv::state_backend::proof_backend::proof::deserialise_proof;
use octez_riscv::state_backend::proof_backend::proof::serialise_proof;
use octez_riscv::state_backend::verify_backend::Verifier;
use octez_riscv::storage;
use octez_riscv::storage::StorageError;
use pointer_apply::ApplyReadOnly;
use pointer_apply::apply_imm;
use pointer_apply::apply_mut;
use sha2::Digest;
use sha2::Sha256;

use crate::storage::Hash;

type OcamlFallible<T> = Result<T, ocaml::Error>;

#[ocaml::sig]
pub struct Repo(PvmStorage);

#[ocaml::sig]
pub type State = ImmutableState<NodePvm<Owned>>;

#[ocaml::sig]
pub type MutState = MutableState<NodePvm<Owned>>;

#[ocaml::sig]
pub struct Id(storage::Hash);

ocaml::custom!(Repo);
ocaml::custom!(Id);

#[derive(ocaml::FromValue, ocaml::ToValue, IntoPrimitive, TryFromPrimitive, strum::EnumCount)]
#[ocaml::sig("Evaluating | Waiting_for_input | Waiting_for_reveal")]
#[repr(u8)]
pub enum Status {
    Evaluating,
    WaitingForInput,
    WaitingForReveal,
}

// Check that [`PvmStatus`] and [`Status`] can be coerced into each other.
const STATUS_ENUM_COERCIBLE: bool = {
    if <PvmStatus as strum::EnumCount>::COUNT != <Status as strum::EnumCount>::COUNT
        || <Status as strum::EnumCount>::COUNT != 3
    {
        panic!("Not coercible!");
    }

    if PvmStatus::Evaluating as u8 != Status::Evaluating as u8 {
        panic!("Not coercible!");
    }

    if PvmStatus::WaitingForInput as u8 != Status::WaitingForInput as u8 {
        panic!("Not coercible!");
    }

    if PvmStatus::WaitingForReveal as u8 != Status::WaitingForReveal as u8 {
        panic!("Not coercible!");
    }

    true
};

impl From<PvmStatus> for Status {
    fn from(item: PvmStatus) -> Self {
        if STATUS_ENUM_COERCIBLE {
            unsafe { std::mem::transmute::<PvmStatus, Self>(item) }
        } else {
            unreachable!()
        }
    }
}

impl From<Status> for PvmStatus {
    fn from(item: Status) -> Self {
        if STATUS_ENUM_COERCIBLE {
            unsafe { std::mem::transmute::<Status, Self>(item) }
        } else {
            unreachable!()
        }
    }
}

/// Wrapper to convert a Rust allocated byte-array to an OCaml allocated Bytes type
pub struct BytesWrapper(Box<[u8]>);

unsafe impl ocaml::ToValue for BytesWrapper {
    fn to_value(&self, _rt: &ocaml::Runtime) -> ocaml::Value {
        unsafe { ocaml::Value::bytes(&self.0) }
    }
}

/// Input values are passed into the PVM after an input request has been made.
/// Analogous to the [`PvmInput`] type.
#[derive(ocaml::FromValue)]
#[ocaml::sig(
    "Inbox_message of {inbox_level: int32; message_counter: int64; payload: bytes} | Reveal of bytes"
)]
pub enum Input<'a> {
    InboxMessage {
        inbox_level: u32,
        message_counter: u64,
        payload: &'a [u8],
    },
    Reveal(&'a [u8]),
}

impl<'a> From<Input<'a>> for PvmInput<'a> {
    fn from(val: Input<'a>) -> Self {
        match val {
            Input::InboxMessage {
                inbox_level,
                message_counter,
                payload,
            } => PvmInput::InboxMessage {
                inbox_level,
                message_counter,
                payload,
            },
            Input::Reveal(data) => PvmInput::Reveal(data),
        }
    }
}

/// Describes possible input requests the PVM may ask for during execution.
#[derive(ocaml::ToValue)]
#[ocaml::sig(
    "No_input_required | Initial | First_after of {level: int32; counter: int64} | Needs_reveal of bytes"
)]
pub enum InputRequest {
    NoInputRequired,
    Initial,
    FirstAfter { level: u32, counter: u64 },
    NeedsReveal(BytesWrapper),
}

impl From<PvmInputRequest> for InputRequest {
    fn from(request: PvmInputRequest) -> Self {
        match request {
            PvmInputRequest::NoInputRequired => InputRequest::NoInputRequired,
            PvmInputRequest::Initial => InputRequest::Initial,
            PvmInputRequest::FirstAfter { level, counter } => {
                InputRequest::FirstAfter { level, counter }
            }
            PvmInputRequest::NeedsReveal(data) => InputRequest::NeedsReveal(BytesWrapper(data)),
        }
    }
}

// This representation guarantees that on the OCaml side, when converting to `Raw_level_repr.t`
// no runtime exceptions will be thrown.
/// Wrapper to convert a Rust unsigned 31 bit value to an OCaml int32 type
pub struct RawLevel(u31);

unsafe impl ocaml::ToValue for RawLevel {
    fn to_value(&self, _rt: &ocaml::Runtime) -> ocaml::Value {
        // Make sure the underlying type is u32 to ensure a no-op conversion to OCaml values
        let wrapped_value: u32 = self.0.value();
        // Make sure the ocaml-rs conversion is for precisely converting into an OCaml i32
        unsafe { ocaml::Value::int32(wrapped_value as i32) }
    }
}

/// Metadata of an output message
#[derive(ocaml::ToValue)]
#[ocaml::sig("{message_index : int64; outbox_level : int32}")]
pub struct OutputInfo {
    pub message_index: u64,
    pub outbox_level: RawLevel,
}

/// A value of this type is generated as part of successfully verifying an output proof.
#[derive(ocaml::ToValue)]
#[ocaml::sig("{info : output_info; encoded_message : bytes}")]
pub struct Output {
    pub info: OutputInfo,
    pub encoded_message: BytesWrapper,
}

// TODO RV-365 Implement OutputProof types
#[ocaml::sig]
pub struct OutputProof;

ocaml::custom!(OutputProof);

/// Hooks for the PVM to call into OCaml code
struct OCamlHooks<F> {
    put_bytes: F,
}

impl<F: Fn(&[u8])> PvmHooks for OCamlHooks<F> {
    fn write_debug_bytes(&mut self, bytes: &[u8]) {
        (self.put_bytes)(bytes);
    }
}

/// Create a PVM hooks instance that calls an OCaml function to print debug information.
fn pvm_hooks_from_ocaml_fn(
    printer: ocaml::Value,
    gc: &ocaml::Runtime,
) -> OCamlHooks<impl Fn(&[u8])> {
    let put_bytes = move |bytes: &[u8]| unsafe {
        ocaml::Value::call1(&printer, gc, bytes).expect("NodeHooks: put_bytes failed");
    };

    OCamlHooks { put_bytes }
}

#[ocaml::func]
#[ocaml::sig("state -> mut_state")]
pub fn octez_riscv_from_imm(state: Pointer<State>) -> Pointer<MutState> {
    state.as_ref().to_mut_state().into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> state")]
pub fn octez_riscv_to_imm(state: Pointer<MutState>) -> Pointer<State> {
    state.as_ref().to_imm_state().into()
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
    id.as_ref().0.into()
}

#[ocaml::func]
#[ocaml::sig("id -> id -> bool")]
pub fn octez_riscv_storage_id_equal(id1: Pointer<Id>, id2: Pointer<Id>) -> bool {
    id1.as_ref().0 == id2.as_ref().0
}

#[ocaml::func]
#[ocaml::sig("state -> state -> bool")]
pub fn octez_riscv_storage_state_equal(state1: Pointer<State>, state2: Pointer<State>) -> bool {
    state1.apply_ro(|pvm1| state2.apply_ro(|pvm2| pvm1 == pvm2))
}

#[ocaml::func]
#[ocaml::sig("unit -> state")]
pub fn octez_riscv_storage_state_empty() -> Pointer<State> {
    ImmutableState::new(NodePvm::empty()).into()
}

#[ocaml::func]
#[ocaml::sig("string -> repo")]
pub fn octez_riscv_storage_load(path: String) -> OcamlFallible<Pointer<Repo>> {
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
) -> OcamlFallible<Pointer<Id>> {
    state.apply_ro(|pvm| match repo.as_mut().0.commit(pvm) {
        Ok(hash) => Ok(Id(hash).into()),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    })
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> state option")]
pub fn octez_riscv_storage_checkout(
    repo: Pointer<Repo>,
    id: Pointer<Id>,
) -> OcamlFallible<Option<Pointer<State>>> {
    let id = &id.as_ref().0;
    match repo.as_ref().0.checkout(id) {
        Ok(pvm) => Ok(Some(ImmutableState::new(pvm).into())),
        Err(PvmStorageError::StorageError(StorageError::NotFound(_))) => Ok(None),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("state -> status")]
pub fn octez_riscv_get_status(state: Pointer<State>) -> Status {
    state.apply_ro(NodePvm::get_status).into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> status")]
pub fn octez_riscv_mut_get_status(state: Pointer<MutState>) -> Status {
    state.apply_ro(NodePvm::get_status).into()
}

#[ocaml::func]
#[ocaml::sig("status -> string")]
pub fn octez_riscv_string_of_status(status: Status) -> String {
    PvmStatus::from(status).to_string()
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_compute_step(state: Pointer<State>) -> Pointer<State> {
    apply_imm(state, |pvm| pvm.compute_step(StdoutDebugHooks)).0
}

#[ocaml::func]
#[ocaml::sig("state -> (bytes -> unit) -> state")]
pub fn octez_riscv_compute_step_with_debug(
    state: Pointer<State>,
    printer: ocaml::Value,
) -> Pointer<State> {
    let hooks = pvm_hooks_from_ocaml_fn(printer, gc);
    apply_imm(state, |pvm| pvm.compute_step(hooks)).0
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (state * int64)")]
pub fn octez_riscv_compute_step_many(
    max_steps: u64,
    state: Pointer<State>,
) -> (Pointer<State>, i64) {
    apply_imm(state, |pvm| {
        pvm.compute_step_many(StdoutDebugHooks, max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("int64 -> mut_state -> int64")]
pub fn octez_riscv_mut_compute_step_many(max_steps: u64, state: Pointer<MutState>) -> i64 {
    apply_mut(state, |pvm| {
        pvm.compute_step_many(StdoutDebugHooks, max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (bytes -> unit) -> (state * int64)")]
pub fn octez_riscv_compute_step_many_with_debug(
    max_steps: u64,
    state: Pointer<State>,
    printer: ocaml::Value,
) -> (Pointer<State>, i64) {
    let hooks = pvm_hooks_from_ocaml_fn(printer, gc);
    apply_imm(state, |pvm| {
        pvm.compute_step_many(hooks, max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("int64 -> mut_state -> (bytes -> unit) -> int64")]
pub fn octez_riscv_mut_compute_step_many_with_debug(
    max_steps: u64,
    state: Pointer<MutState>,
    printer: ocaml::Value,
) -> i64 {
    let hooks = pvm_hooks_from_ocaml_fn(printer, gc);
    apply_mut(state, |pvm| {
        pvm.compute_step_many(hooks, max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_tick(state: Pointer<State>) -> u64 {
    state.apply_ro(NodePvm::get_tick)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int64")]
pub fn octez_riscv_mut_get_tick(state: Pointer<MutState>) -> u64 {
    state.apply_ro(NodePvm::get_tick)
}

#[ocaml::func]
#[ocaml::sig("state -> int32 option")]
pub fn octez_riscv_get_level(state: Pointer<State>) -> Option<u32> {
    state.apply_ro(NodePvm::get_current_level)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int32 option")]
pub fn octez_riscv_mut_get_level(state: Pointer<MutState>) -> Option<u32> {
    state.apply_ro(NodePvm::get_current_level)
}

fn verify_checksum(contents: &[u8], checksum: &str) -> bool {
    let mut hasher = Sha256::new();
    hasher.update(contents);
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
    // after checking consistency with the provided checksum.
    // "kernel:<path to kernel>:<kernel checksum>"
    // Any string not matching this format will be treated as an actual kernel to be installed.
    let install_kernel = |pvm: &mut NodePvm| {
        if let Ok(boot_sector) = str::from_utf8(boot_sector) {
            let parts: Vec<&str> = boot_sector.split(':').collect();
            if let ["kernel", kernel_path, kernel_checksum] = parts.as_slice() {
                let kernel = read_boot_sector_binary(kernel_path, kernel_checksum);
                return pvm.install_boot_sector(&kernel);
            } else {
                return pvm.install_boot_sector(boot_sector.as_bytes());
            }
        }
        pvm.install_boot_sector(boot_sector);
    };

    apply_imm(state, install_kernel).0
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_state_hash(state: Pointer<State>) -> [u8; 32] {
    state.apply_ro(NodePvm::hash).into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes")]
pub fn octez_riscv_mut_state_hash(state: Pointer<MutState>) -> [u8; 32] {
    state.apply_ro(NodePvm::hash).into()
}

#[ocaml::func]
#[ocaml::sig("state -> input -> state")]
pub fn octez_riscv_set_input(state: Pointer<State>, input: Input) -> Pointer<State> {
    apply_imm(state, |pvm| NodePvm::set_input(pvm, input.into())).0
}

#[ocaml::func]
#[ocaml::sig("mut_state -> input -> unit")]
pub fn octez_riscv_mut_set_input(state: Pointer<MutState>, input: Input) -> () {
    let res = apply_mut(state, |pvm| NodePvm::set_input(pvm, input.into()));
    assert!(res)
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_message_counter(state: Pointer<State>) -> u64 {
    state.apply_ro(NodePvm::get_message_counter)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int64")]
pub fn octez_riscv_mut_get_message_counter(state: Pointer<MutState>) -> u64 {
    state.apply_ro(NodePvm::get_message_counter)
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
        unsafe { ocaml::Value::hash_variant(gc, "Msg", Some(s.to_value(gc))) }
    })
}

/// Proof type used by both the protocol and the rollup node.
#[ocaml::sig]
struct Proof {
    final_state_hash: Hash,
    serialised_proof: Box<[u8]>,
    /// When deserialising a proof in the protocol, a verifier backend and its [`MerkleProof`] are needed for the rest of the API.
    verifier: Option<(NodePvm<Verifier>, MerkleProof)>,
}

impl From<PvmProof> for Proof {
    fn from(proof: PvmProof) -> Self {
        Proof {
            final_state_hash: proof.final_state_hash(),
            serialised_proof: serialise_proof(&proof).collect(),
            verifier: None,
        }
    }
}

impl Proof {
    /// Obtain the [`NodePvm`] and [`MerkleProof`] for this proof.
    ///
    /// Create them from the raw proof bytes if they do not exist yet.
    fn get_or_create_verifier(&mut self) -> Result<(&NodePvm<Verifier>, &MerkleProof), String> {
        match &mut self.verifier {
            Some((node_pvm, merkle_tree)) => Ok((node_pvm, merkle_tree)),

            other => {
                let (octez_riscv_proof, node_pvm) =
                    deserialise_proof(self.serialised_proof.iter().copied())
                        .map_err(|e| e.to_string())?;
                let merkle_tree = octez_riscv_proof.into_tree();
                let (node_pvm, merkle_tree) = other.get_or_insert((node_pvm, merkle_tree));
                Ok((node_pvm, merkle_tree))
            }
        }
    }
}

ocaml::custom!(Proof);

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_proof_start_state(mut proof: Pointer<Proof>) -> OcamlFallible<[u8; 32]> {
    // Ensure that the proof has been deserialised and contains the verifier backend.
    let (_, merkle_proof) = match proof.as_mut().get_or_create_verifier() {
        Ok(verifier) => verifier,
        Err(e) => {
            return Err(ocaml::Error::Error(
                format!("Error getting start state hash from proof: {e}").into(),
            ));
        }
    };

    match merkle_proof.root_hash() {
        Ok(hash) => Ok(hash.into()),
        Err(e) => {
            return Err(ocaml::Error::Error(
                format!("Error getting start state hash from proof: {e}").into(),
            ));
        }
    }
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_proof_stop_state(proof: Pointer<Proof>) -> [u8; 32] {
    proof.as_ref().final_state_hash.into()
}

#[ocaml::func]
#[ocaml::sig("input option -> state -> proof option")]
pub fn octez_riscv_produce_proof(
    input: Option<Input>,
    state: Pointer<State>,
) -> Option<Pointer<Proof>> {
    let input = input.map(|i| i.into());
    let proof = state.apply_ro(|pvm| NodePvm::produce_proof(pvm, input, NoHooks));
    proof.map(|proof| Pointer::from(Proof::from(proof)))
}

#[ocaml::func]
#[ocaml::sig("input option -> proof -> input_request option")]
pub fn octez_riscv_verify_proof(
    input: Option<Input>,
    mut proof: Pointer<Proof>,
) -> Option<InputRequest> {
    let input = input.map(|i| i.into());

    let final_state_hash = proof.as_ref().final_state_hash;
    let (node_pvm, merkle_tree) = proof.as_mut().get_or_create_verifier().ok()?;
    let input_request =
        node_pvm
            .clone()
            .verify_proof(merkle_tree, &final_state_hash, input, NoHooks)?;

    Some(InputRequest::from(input_request))
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub unsafe fn octez_riscv_serialise_proof(proof: Pointer<Proof>) -> BytesWrapper {
    // Safety: the function needs to return the same `ocaml::Value` as in the signature.
    // In this case, an OCaml bytes value has to be returned.
    let bytes = proof.as_ref().serialised_proof.clone();
    BytesWrapper(bytes)
}

#[ocaml::func]
#[ocaml::sig("bytes -> (proof, string) Result.t")]
pub fn octez_riscv_deserialise_proof(bytes: &[u8]) -> Result<Pointer<Proof>, String> {
    let iter = bytes.iter().cloned();

    let (proof, verifier) = deserialise_proof(iter).map_err(|e| e.to_string())?;

    Ok(Proof {
        final_state_hash: proof.final_state_hash(),
        serialised_proof: bytes.into(),
        verifier: Some((verifier, proof.into_tree())),
    }
    .into())
}

#[ocaml::func]
#[ocaml::sig("output_proof -> output_info")]
pub fn octez_riscv_output_info_of_output_proof(
    _output_proof: Pointer<OutputProof>,
) -> Result<OutputInfo, String> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> bytes")]
pub fn octez_riscv_state_of_output_proof(_output_proof: Pointer<OutputProof>) -> [u8; 32] {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> output option")]
pub fn octez_riscv_verify_output_proof(_output_proof: Pointer<OutputProof>) -> Option<Output> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> bytes")]
pub fn octez_riscv_serialise_output_proof(_output_proof: Pointer<OutputProof>) -> ocaml::Value {
    // TODO RV-365 Implement Output & OutputProof types
    // Ue similar implementation to octez_riscv_serialise_proof
    todo!()
}

#[ocaml::func]
#[ocaml::sig("bytes -> (output_proof, string) result")]
pub fn octez_riscv_deserialise_output_proof(_bytes: &[u8]) -> Result<Pointer<OutputProof>, String> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_get_reveal_request(state: Pointer<State>) -> BytesWrapper {
    let serialised_reveal_request: Vec<u8> = state.apply_ro(NodePvm::get_reveal_request);
    BytesWrapper(serialised_reveal_request.into_boxed_slice())
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes")]
pub fn octez_riscv_mut_get_reveal_request(state: Pointer<MutState>) -> BytesWrapper {
    let serialised_reveal_request: Vec<u8> = state.apply_ro(NodePvm::get_reveal_request);
    BytesWrapper(serialised_reveal_request.into_boxed_slice())
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_insert_failure(state: Pointer<State>) -> Pointer<State> {
    apply_imm(state, |pvm| pvm.insert_failure()).0
}

#[ocaml::func]
#[ocaml::sig("mut_state -> unit")]
pub fn octez_riscv_mut_insert_failure(state: Pointer<MutState>) {
    apply_mut(state, |pvm| pvm.insert_failure())
}
