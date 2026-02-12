// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines the OCaml API for the RISC-V PVM and serves as a basis for building the octez-riscv-api OCaml library.

use core::panic;
use std::collections::LinkedList;
use std::fs;
use std::str;

use arbitrary_int::u31;
use derive_more::Deref;
use derive_more::DerefMut;
use ocaml::ToValue;
use octez_riscv::machine_state::page_cache::EmptyPageCache;
use octez_riscv::pvm::InputRequest as PvmInputRequest;
use octez_riscv::pvm::PvmInput;
use octez_riscv::pvm::PvmStatus;
use octez_riscv::pvm::hooks::NoHooks;
use octez_riscv::pvm::hooks::PvmHooks;
use octez_riscv::pvm::hooks::StdoutDebugHooks;
use octez_riscv::pvm::node_pvm::NodePvm;
use octez_riscv::pvm::node_pvm::PvmStorage;
use octez_riscv::pvm::node_pvm::PvmStorageError;
use octez_riscv::pvm::outbox::OutboxMessage;
use octez_riscv::pvm::outbox::Output as PvmOutput;
use octez_riscv::pvm::outbox::OutputInfo as PvmOutputInfo;
use octez_riscv::state_backend::proof_backend::proof::Proof as PvmProof;
use octez_riscv::state_backend::proof_backend::proof::deserialise_proof;
use octez_riscv::state_backend::proof_backend::proof::serialise_proof;
use octez_riscv::storage::StorageError;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::ImmutableState;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::hash;
use octez_riscv_data::merkle_proof::proof_tree::MerkleProof;
use octez_riscv_data::mode::Normal;
use octez_riscv_data::mode::Verify;
use parking_lot::Mutex;
use parking_lot::RwLock;
use sha2::Digest;
use sha2::Sha256;

/// Wrapper for `NodePvm<Normal>` to enable
/// customing OCaml GC's resource tracking.
#[derive(Deref, DerefMut)]
#[repr(transparent)]
pub struct NodePvmState(NodePvm<Normal>);

impl TryClone for NodePvmState {
    type Error = ocaml::Error;

    fn try_clone(&self) -> Result<Self, Self::Error> {
        NodePvm::try_clone(self)
            .map_err(|e| ocaml::Error::Error(Box::new(e)))
            .map(Self)
    }
}

impl CustomGcResource for NodePvmState {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.node_pvm_state";

    const MUTABLE_NAME: &'static str = "riscv.mut.node_pvm_state";

    const IMMUTABLE_USED: usize = 1;

    const IMMUTABLE_MAX: usize = 2;

    const MUTABLE_USED: usize = 1;

    const MUTABLE_MAX: usize = 2;
}

#[ocaml::sig]
pub struct Repo(RwLock<PvmStorage>);

#[ocaml::sig]
pub type State = ImmutableState<NodePvmState>;

#[ocaml::sig]
pub type MutState = MutableState<NodePvmState>;

#[ocaml::sig]
pub struct Id(hash::Hash);

ocaml::custom!(Repo);
ocaml::custom!(Id);

#[derive(ocaml::FromValue, ocaml::ToValue, strum::EnumCount)]
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

unsafe impl ocaml::FromValue for BytesWrapper {
    fn from_value(value: ocaml::Value) -> Self {
        // SAFETY: The ToValue implementation for this type uses the OCaml bytes type.
        // and ocaml-rs will only call this function on an OCaml value coming from BytesWrapper.
        let bytes: &[u8] = unsafe { value.bytes_val() };
        BytesWrapper(bytes.into())
    }
}

/// Input values are passed into the PVM after an input request has been made.
/// Analogous to the [`PvmInput`] type.
#[derive(ocaml::FromValue, ocaml::ToValue)]
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
#[derive(ocaml::ToValue, ocaml::FromValue)]
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

unsafe impl ocaml::FromValue for RawLevel {
    fn from_value(value: ocaml::Value) -> Self {
        // SAFETY: The ToValue implementation for this type uses the OCaml int32 type
        // and ocaml-rs will only call this function on an OCaml value coming from RawLevel.
        let wrapped_value: u32 = unsafe { value.int32_val() as u32 };
        RawLevel(u31::new(wrapped_value))
    }
}

/// Metadata of an output message
#[derive(ocaml::ToValue, ocaml::FromValue)]
#[ocaml::sig("{message_index : int32; outbox_level : int32}")]
pub struct OutputInfo {
    pub message_index: u32,
    pub outbox_level: RawLevel,
}

impl From<PvmOutputInfo> for OutputInfo {
    fn from(output_info: PvmOutputInfo) -> Self {
        Self {
            message_index: output_info.index,
            outbox_level: RawLevel(u31::new(output_info.level)),
        }
    }
}

impl From<OutputInfo> for PvmOutputInfo {
    fn from(output_info: OutputInfo) -> Self {
        Self {
            level: output_info.outbox_level.0.into(),
            index: output_info.message_index,
        }
    }
}

/// A value of this type is generated as part of successfully verifying an output proof.
#[derive(ocaml::ToValue, ocaml::FromValue)]
#[ocaml::sig("{info : output_info; encoded_message : bytes}")]
pub struct Output {
    pub info: OutputInfo,
    pub encoded_message: BytesWrapper,
}

impl From<PvmOutput> for Output {
    fn from(output: PvmOutput) -> Self {
        Self {
            info: output.info.into(),
            encoded_message: BytesWrapper(output.message.into()),
        }
    }
}

impl TryFrom<Output> for PvmOutput {
    type Error = String;

    fn try_from(output: Output) -> Result<Self, String> {
        Ok(Self {
            message: OutboxMessage::try_from(output.encoded_message.0)
                .map_err(|e| e.to_string())?,
            info: output.info.into(),
        })
    }
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
pub fn octez_riscv_from_imm(state: SafePointer<State>) -> SafePointer<MutState> {
    state.to_mut_state().into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> state")]
pub fn octez_riscv_to_imm(state: SafePointer<MutState>) -> OcamlFallible<SafePointer<State>> {
    Ok(state.to_imm_state()?.into())
}

#[ocaml::func]
#[ocaml::sig("bytes -> id")]
pub fn octez_riscv_id_unsafe_of_raw_bytes(bytes: &[u8]) -> SafePointer<Id> {
    let digest: [u8; hash::Hash::DIGEST_SIZE] = bytes.try_into().expect("Invalid hash length");
    let hash = hash::Hash::from(digest);
    Id(hash).into()
}

#[ocaml::func]
#[ocaml::sig("id -> bytes")]
pub fn octez_riscv_storage_id_to_raw_bytes(id: SafePointer<Id>) -> [u8; 32] {
    id.0.into()
}

#[ocaml::func]
#[ocaml::sig("id -> id -> bool")]
pub fn octez_riscv_storage_id_equal(id1: SafePointer<Id>, id2: SafePointer<Id>) -> bool {
    id1.0 == id2.0
}

#[ocaml::func]
#[ocaml::sig("state -> state -> bool")]
pub fn octez_riscv_storage_state_equal(
    state1: SafePointer<State>,
    state2: SafePointer<State>,
) -> bool {
    state1.apply_ro(|pvm1| state2.apply_ro(|pvm2| pvm1 == pvm2))
}

#[ocaml::func]
#[ocaml::sig("mut_state -> mut_state -> bool")]
pub fn octez_riscv_storage_mut_state_equal(
    state1: SafePointer<MutState>,
    state2: SafePointer<MutState>,
) -> bool {
    state1.apply_ro(|pvm1| state2.apply_ro(|pvm2| pvm1 == pvm2))
}

#[ocaml::func]
#[ocaml::sig("unit -> state")]
pub fn octez_riscv_storage_state_empty() -> SafePointer<State> {
    ImmutableState::new(NodePvmState(NodePvm::empty())).into()
}

#[ocaml::func]
#[ocaml::sig("unit -> mut_state")]
pub fn octez_riscv_storage_mut_state_empty() -> SafePointer<MutState> {
    MutableState::owned(NodePvmState(NodePvm::empty())).into()
}

#[ocaml::func]
#[ocaml::sig("string -> repo")]
pub fn octez_riscv_storage_load(path: String) -> OcamlFallible<SafePointer<Repo>> {
    match PvmStorage::load(path) {
        Ok(repo) => Ok(Repo(RwLock::new(repo)).into()),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("repo -> unit")]
pub fn octez_riscv_storage_close(_repo: SafePointer<Repo>) {}

#[ocaml::func]
#[ocaml::sig("repo -> mut_state -> id")]
pub fn octez_riscv_storage_commit(
    repo: SafePointer<Repo>,
    state: SafePointer<MutState>,
) -> OcamlFallible<SafePointer<Id>> {
    state.apply_ro(|pvm| {
        let mut guard = repo.0.write();
        match guard.commit(pvm) {
            Ok(hash) => Ok(Id(hash).into()),
            Err(e) => Err(ocaml::Error::Error(Box::new(e))),
        }
    })
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> mut_state option")]
pub fn octez_riscv_storage_checkout(
    repo: SafePointer<Repo>,
    id: SafePointer<Id>,
) -> OcamlFallible<Option<SafePointer<MutState>>> {
    let id = &id.0;
    let guard = repo.0.read();
    match guard.checkout(id) {
        Ok(pvm) => Ok(Some(MutableState::owned(NodePvmState(pvm)).into())),
        Err(PvmStorageError::StorageError(StorageError::NotFound(_))) => Ok(None),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("state -> status")]
pub fn octez_riscv_get_status(state: SafePointer<State>) -> Status {
    state.apply_ro(NodePvm::get_status).into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> status")]
pub fn octez_riscv_mut_get_status(state: SafePointer<MutState>) -> Status {
    state.apply_ro(NodePvm::get_status).into()
}

#[ocaml::func]
#[ocaml::sig("status -> string")]
pub fn octez_riscv_string_of_status(status: Status) -> String {
    PvmStatus::from(status).to_string()
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_compute_step(state: SafePointer<State>) -> OcamlFallible<SafePointer<State>> {
    Ok(state.apply_imm(|pvm| pvm.compute_step(StdoutDebugHooks))?.0)
}

#[ocaml::func]
#[ocaml::sig("state -> (bytes -> unit) -> state")]
pub fn octez_riscv_compute_step_with_debug(
    state: SafePointer<State>,
    printer: ocaml::Value,
) -> OcamlFallible<SafePointer<State>> {
    let hooks = pvm_hooks_from_ocaml_fn(printer, gc);
    Ok(state.apply_imm(|pvm| pvm.compute_step(hooks))?.0)
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (state * int64)")]
pub fn octez_riscv_compute_step_many(
    max_steps: u64,
    state: SafePointer<State>,
) -> OcamlFallible<(SafePointer<State>, i64)> {
    state.apply_imm(|pvm| pvm.compute_step_many(StdoutDebugHooks, max_steps as usize))
}

#[ocaml::func]
#[ocaml::sig("int64 -> mut_state -> int64")]
pub fn octez_riscv_mut_compute_step_many(
    max_steps: u64,
    state: SafePointer<MutState>,
) -> OcamlFallible<i64> {
    state.apply(|pvm| pvm.compute_step_many(StdoutDebugHooks, max_steps as usize))
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (bytes -> unit) -> (state * int64)")]
pub fn octez_riscv_compute_step_many_with_debug(
    max_steps: u64,
    state: SafePointer<State>,
    printer: ocaml::Value,
) -> OcamlFallible<(SafePointer<State>, i64)> {
    let hooks = pvm_hooks_from_ocaml_fn(printer, gc);
    state.apply_imm(|pvm| pvm.compute_step_many(hooks, max_steps as usize))
}

#[ocaml::func]
#[ocaml::sig("int64 -> mut_state -> (bytes -> unit) -> int64")]
pub fn octez_riscv_mut_compute_step_many_with_debug(
    max_steps: u64,
    state: SafePointer<MutState>,
    printer: ocaml::Value,
) -> OcamlFallible<i64> {
    let hooks = pvm_hooks_from_ocaml_fn(printer, gc);
    state.apply(|pvm| pvm.compute_step_many(hooks, max_steps as usize))
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_tick(state: SafePointer<State>) -> u64 {
    state.apply_ro(NodePvm::get_tick)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int64")]
pub fn octez_riscv_mut_get_tick(state: SafePointer<MutState>) -> u64 {
    state.apply_ro(NodePvm::get_tick)
}

#[ocaml::func]
#[ocaml::sig("state -> int32 option")]
pub fn octez_riscv_get_level(state: SafePointer<State>) -> Option<u32> {
    state.apply_ro(NodePvm::get_current_level)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int32 option")]
pub fn octez_riscv_mut_get_level(state: SafePointer<MutState>) -> Option<u32> {
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

// RISC-V kernels are too large to be originated directly. In order to
// temporarily bypass this limitation (TODO: RV-109 Port kernel installer to RISC-V)
// the boot sector is installed by loading it from a path passed at origination
// after checking consistency with the provided checksum.
// "kernel:<path to kernel>:<kernel checksum>"
// Any string not matching this format will be treated as an actual kernel to be installed.
fn install_boot_sector(pvm: &mut NodePvm, boot_sector: &[u8]) {
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
}

#[ocaml::func]
#[ocaml::sig("state -> bytes -> state")]
pub fn octez_riscv_install_boot_sector(
    state: SafePointer<State>,
    boot_sector: &[u8],
) -> OcamlFallible<SafePointer<State>> {
    Ok(state
        .apply_imm(|pvm| install_boot_sector(pvm, boot_sector))?
        .0)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes -> unit")]
pub fn octez_riscv_mut_install_boot_sector(
    state: SafePointer<MutState>,
    boot_sector: &[u8],
) -> OcamlFallible<()> {
    state.apply(|pvm| install_boot_sector(pvm, boot_sector))
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_state_hash(state: SafePointer<State>) -> [u8; 32] {
    state.apply_ro(NodePvm::hash).into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes")]
pub fn octez_riscv_mut_state_hash(state: SafePointer<MutState>) -> [u8; 32] {
    state.apply_ro(NodePvm::hash).into()
}

#[ocaml::func]
#[ocaml::sig("state -> input -> state")]
pub fn octez_riscv_set_input(
    state: SafePointer<State>,
    input: Input,
) -> OcamlFallible<SafePointer<State>> {
    Ok(state
        .apply_imm(|pvm| NodePvm::set_input(pvm, input.into()))?
        .0)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> input -> unit")]
pub fn octez_riscv_mut_set_input(state: SafePointer<MutState>, input: Input) -> OcamlFallible<()> {
    let res = state.apply(|pvm| NodePvm::set_input(pvm, input.into()))?;
    assert!(res);
    Ok(())
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_message_counter(state: SafePointer<State>) -> u64 {
    state.apply_ro(NodePvm::get_message_counter)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int64")]
pub fn octez_riscv_mut_get_message_counter(state: SafePointer<MutState>) -> u64 {
    state.apply_ro(NodePvm::get_message_counter)
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> string -> (unit, [`Msg of string]) result")]
pub unsafe fn octez_riscv_storage_export_snapshot(
    repo: SafePointer<Repo>,
    id: SafePointer<Id>,
    path: &str,
) -> Result<(), ocaml::Value> {
    let id = &id.0;
    let guard = repo.0.read();
    guard.export_snapshot(id, path).map_err(|e| {
        let s = format!("{e:?}");
        unsafe { ocaml::Value::hash_variant(gc, "Msg", Some(s.to_value(gc))) }
    })
}

/// Proof type used by both the protocol and the rollup node.
#[ocaml::sig]
struct Proof {
    final_state_hash: hash::Hash,
    serialised_proof: Box<[u8]>,
    /// When deserialising a proof in the protocol, a verifier backend and its [`MerkleProof`] are needed for the rest of the API.
    verifier: Mutex<Option<(NodePvm<Verify, EmptyPageCache>, MerkleProof)>>,
}

impl From<PvmProof> for Proof {
    fn from(proof: PvmProof) -> Self {
        Proof {
            final_state_hash: proof.final_state_hash(),
            serialised_proof: serialise_proof(&proof).into_boxed_slice(),
            verifier: Mutex::new(None),
        }
    }
}

impl Proof {
    /// Run a closure over the [`NodePvm`] and [`MerkleProof`] for this proof.
    ///
    /// Create them from the raw proof bytes if they do not exist.
    fn with_verifier<R>(
        &self,
        f: impl FnOnce(&(NodePvm<Verify, EmptyPageCache>, MerkleProof)) -> R,
    ) -> Result<R, String> {
        let mut guard = self.verifier.lock();
        if guard.is_none() {
            let (proof, node_pvm) = deserialise_proof(self.serialised_proof.iter().copied())
                .map_err(|e| e.to_string())?;
            *guard = Some((node_pvm, proof.into_tree()));
        }
        Ok(f(guard.as_ref().expect(
            "The verifier either already existed or has just been instantiated",
        )))
    }

    /// Obtain the [`NodePvm`] and [`MerkleProof`] for this proof.
    ///
    /// Create them from the raw proof bytes if they do not exist.
    fn take_or_create_verifier(
        &self,
    ) -> Result<(NodePvm<Verify, EmptyPageCache>, MerkleProof), String> {
        if let Some(verifier) = self.verifier.lock().take() {
            return Ok(verifier);
        }

        let (proof, node_pvm) =
            deserialise_proof(self.serialised_proof.iter().copied()).map_err(|e| e.to_string())?;
        Ok((node_pvm, proof.into_tree()))
    }
}

ocaml::custom!(Proof);

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_proof_start_state(proof: SafePointer<Proof>) -> OcamlFallible<[u8; 32]> {
    proof
        .with_verifier(|(_, merkle_proof)| merkle_proof.root_hash().into())
        .map_err(|e| {
            ocaml::Error::Error(format!("Error getting start state hash from proof: {e}").into())
        })
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_proof_stop_state(proof: SafePointer<Proof>) -> [u8; 32] {
    proof.final_state_hash.into()
}

#[ocaml::func]
#[ocaml::sig("input option -> state -> proof option")]
pub fn octez_riscv_produce_proof(
    input: Option<Input>,
    state: SafePointer<State>,
) -> Option<SafePointer<Proof>> {
    let input = input.map(|i| i.into());
    let proof = state.apply_ro(|pvm| NodePvm::produce_proof(pvm, input, NoHooks));
    proof.map(|proof| SafePointer::from(Proof::from(proof)))
}

#[ocaml::func]
#[ocaml::sig("input option -> proof -> input_request option")]
pub fn octez_riscv_verify_proof(
    input: Option<Input>,
    proof: SafePointer<Proof>,
) -> OcamlFallible<Option<InputRequest>> {
    let input = input.map(|i| i.into());
    let final_state_hash = proof.final_state_hash;

    let Ok((mut node_pvm, merkle_tree)) = proof.take_or_create_verifier() else {
        return Ok(None);
    };

    Ok(node_pvm
        .verify_proof(&merkle_tree, &final_state_hash, input, NoHooks)
        .map(InputRequest::from))
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub unsafe fn octez_riscv_serialise_proof(proof: SafePointer<Proof>) -> BytesWrapper {
    // Safety: the function needs to return the same `ocaml::Value` as in the signature.
    // In this case, an OCaml bytes value has to be returned.
    let bytes = proof.serialised_proof.clone();
    BytesWrapper(bytes)
}

#[ocaml::func]
#[ocaml::sig("bytes -> (proof, string) Result.t")]
pub fn octez_riscv_deserialise_proof(bytes: &[u8]) -> Result<SafePointer<Proof>, String> {
    let iter = bytes.iter().copied();

    let (proof, node_pvm): (PvmProof, NodePvm<Verify, EmptyPageCache>) =
        deserialise_proof(iter).map_err(|e| e.to_string())?;
    let final_state_hash = proof.final_state_hash();
    let merkle_tree = proof.into_tree();

    Ok(Proof {
        final_state_hash,
        serialised_proof: bytes.into(),
        verifier: Mutex::new(Some((node_pvm, merkle_tree))),
    }
    .into())
}

#[ocaml::func]
#[ocaml::sig("output_proof -> output_info")]
pub fn octez_riscv_output_info_of_output_proof(
    _output_proof: SafePointer<OutputProof>,
) -> Result<OutputInfo, String> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> bytes")]
pub fn octez_riscv_state_of_output_proof(_output_proof: SafePointer<OutputProof>) -> [u8; 32] {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> output option")]
pub fn octez_riscv_verify_output_proof(_output_proof: SafePointer<OutputProof>) -> Option<Output> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> bytes")]
pub fn octez_riscv_serialise_output_proof(_output_proof: SafePointer<OutputProof>) -> ocaml::Value {
    // TODO RV-365 Implement Output & OutputProof types
    // Ue similar implementation to octez_riscv_serialise_proof
    todo!()
}

#[ocaml::func]
#[ocaml::sig("bytes -> (output_proof, string) result")]
pub fn octez_riscv_deserialise_output_proof(
    _bytes: &[u8],
) -> Result<SafePointer<OutputProof>, String> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("state -> int32 -> output list")]
pub fn octez_riscv_get_outbox_for_level(
    state: SafePointer<State>,
    level: RawLevel,
) -> LinkedList<Output> {
    // TODO: RV-918: return Iterator for get_outbox_messages to avoid double allocation
    let outbox_messages = state
        .apply_ro(|state| NodePvm::get_outbox_messages(state, level.0.into()))
        .unwrap_or_default();
    outbox_messages.into_iter().map(|o| o.into()).collect()
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_get_reveal_request(state: SafePointer<State>) -> BytesWrapper {
    let serialised_reveal_request: Vec<u8> = state.apply_ro(NodePvm::get_reveal_request);
    BytesWrapper(serialised_reveal_request.into_boxed_slice())
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes")]
pub fn octez_riscv_mut_get_reveal_request(state: SafePointer<MutState>) -> BytesWrapper {
    let serialised_reveal_request: Vec<u8> = state.apply_ro(NodePvm::get_reveal_request);
    BytesWrapper(serialised_reveal_request.into_boxed_slice())
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_insert_failure(state: SafePointer<State>) -> OcamlFallible<SafePointer<State>> {
    Ok(state.apply_imm(|pvm| pvm.insert_failure())?.0)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> unit")]
pub fn octez_riscv_mut_insert_failure(state: SafePointer<MutState>) -> OcamlFallible<()> {
    state.apply(|pvm| pvm.insert_failure())
}

/// Test endpoint to check argument passing between OCaml and Rust.
#[doc(hidden)]
#[ocaml::func]
#[ocaml::sig("status -> status")]
pub fn octez_riscv_test_status(status: Status) -> Status {
    status
}

/// Test endpoint to check argument passing between OCaml and Rust.
#[doc(hidden)]
#[ocaml::func]
#[ocaml::sig("input -> input")]
pub fn octez_riscv_test_input(input: Input) -> Input<'static> {
    // Ideally we would want to just pass `input` as is, but due to lifetime/ocaml-rs limitations,
    // string slices can't be easily passed. As a workaround we form a new input based on the original input,
    // but with the string slices being hard coded - matching the ones in `test_rust_bindings.rs`.
    // The asserts in this function test the OCaml -> Rust conversion.
    // On the OCaml side, the hard coded inputs are tested in `test_rust_bindings.ml`.
    match input {
        Input::InboxMessage {
            inbox_level,
            message_counter,
            payload,
        } => {
            assert_eq!(payload, b"inbox_payload");
            Input::InboxMessage {
                inbox_level,
                message_counter,
                payload: b"inbox_payload",
            }
        }
        Input::Reveal(payload) => {
            assert_eq!(payload, b"reveal_payload");
            Input::Reveal(b"reveal_payload")
        }
    }
}

/// Test endpoint to check argument passing between OCaml and Rust.
#[doc(hidden)]
#[ocaml::func]
#[ocaml::sig("input_request -> input_request")]
pub fn octez_riscv_test_input_request(input_request: InputRequest) -> InputRequest {
    input_request
}

/// Test endpoint to check argument passing between OCaml and Rust.
#[doc(hidden)]
#[ocaml::func]
#[ocaml::sig("output_info -> output_info")]
pub fn octez_riscv_test_output_info(output_info: OutputInfo) -> OutputInfo {
    output_info
}

// TODO: RV-922: Remove redundant type check tests when E2E testing
// is complete
/// Test endpoint to check argument passing between OCaml and Rust.
#[doc(hidden)]
#[ocaml::func]
#[ocaml::sig("output -> output")]
pub fn octez_riscv_test_output(output: Output) -> Output {
    output
}

/// Test endpoint to check argument passing between OCaml and Rust.
#[doc(hidden)]
#[ocaml::func]
#[ocaml::sig("output list -> output list")]
pub fn octez_riscv_test_output_list(outputs: LinkedList<Output>) -> LinkedList<Output> {
    outputs
}
