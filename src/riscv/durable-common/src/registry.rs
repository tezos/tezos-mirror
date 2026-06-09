// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generic registry state wrapper for OCaml GC resource tracking.

use std::convert::Infallible;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::panic::AssertUnwindSafe;
use std::sync::mpsc::SyncSender;
use std::sync::mpsc::sync_channel;
use std::thread::JoinHandle;

use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::merkle_proof::FromProof;
use octez_riscv_data::merkle_proof::proof_tree::MerkleProof;
use octez_riscv_data::merkle_proof::proof_tree::ProofPart;
use octez_riscv_data::mode::Mode;
use octez_riscv_data::mode::Normal;
use octez_riscv_data::mode::ProvableExt;
use octez_riscv_data::mode::Prove;
use octez_riscv_data::mode::Verify;
use octez_riscv_data::mode::utils::NotFound;
use octez_riscv_data::mode::utils::catch_not_found;
use octez_riscv_durable_storage::errors::OperationalError;
use octez_riscv_durable_storage::registry::Registry;
use octez_riscv_durable_storage::storage::KeyValueStore;

use crate::api_common::BackgroundKeyValueStore;
use crate::api_common::RegistryApply;

/// Type alias for a mutable registry state backed by a generic key-value store.
pub type DsRegistry<KV, G> = MutableState<RegistryState<KV, G>>;

/// Type alias for a prove-mode registry state backed by a generic key-value store.
pub type DsProveRegistry<KV, G> = BackgroundRegistry<KV, G, Prove<'static>>;

/// Type alias for a verify-mode registry state backed by a generic key-value store.
pub type DsVerifyRegistry<KV, G> = BackgroundRegistry<KV, G, Verify>;

/// Marker trait supplying OCaml GC resource names.
pub trait GcNames: Send + Sync + 'static {
    /// Name used to register the immutable (read-only) OCaml custom block.
    const IMMUTABLE_NAME: &'static str;
    /// Name used to register the mutable OCaml custom block.
    const MUTABLE_NAME: &'static str;
}

/// Wrapper to enable customizing OCaml GC's resource tracking.
#[repr(transparent)]
pub struct RegistryState<KV: KeyValueStore, G> {
    inner: Registry<KV, Normal>,
    _phantom: PhantomData<G>,
}

impl<KV: BackgroundKeyValueStore, G> RegistryState<KV, G> {
    /// Construct a new registry.
    pub fn new(repo: KV::Repo) -> Result<Self, OperationalError> {
        let reg = Registry::new(repo)?;
        Ok(Self {
            inner: reg,
            _phantom: PhantomData,
        })
    }
}

impl<KV: KeyValueStore, G> From<Registry<KV, Normal>> for RegistryState<KV, G> {
    fn from(value: Registry<KV, Normal>) -> Self {
        Self {
            inner: value,
            _phantom: PhantomData,
        }
    }
}

impl<KV: KeyValueStore, G> Deref for RegistryState<KV, G> {
    type Target = Registry<KV, Normal>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<KV: KeyValueStore, G> DerefMut for RegistryState<KV, G> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<KV, G> TryClone for RegistryState<KV, G>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone,
{
    type Error = OperationalError;

    fn try_clone(&self) -> Result<Self, Self::Error> {
        Ok(RegistryState {
            inner: self.inner.try_clone()?,
            _phantom: PhantomData,
        })
    }
}

impl<KV: KeyValueStore, G: GcNames> CustomGcResource for RegistryState<KV, G> {
    const IMMUTABLE_NAME: &'static str = G::IMMUTABLE_NAME;
    const MUTABLE_NAME: &'static str = G::MUTABLE_NAME;
}

impl<KV, G> RegistryApply<KV, Normal> for DsRegistry<KV, G>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
{
    type NotFoundError = Infallible;

    fn apply<F, R>(&self, fun: F) -> Result<Result<R, Infallible>, OperationalError>
    where
        F: FnOnce(&mut Registry<KV, Normal>) -> R + Send + 'static,
        KV::Repo: Clone,
    {
        Ok(Ok(MutableState::apply(self, fun)?))
    }

    fn apply_ro<F, R>(&self, fun: F) -> Result<Result<R, Infallible>, OperationalError>
    where
        F: FnOnce(&Registry<KV, Normal>) -> R + Send + 'static,
    {
        Ok(Ok(MutableState::apply_ro(self, fun)))
    }
}

/// Command being sent to the background registry
type DynCommand<KV, M> = dyn FnOnce(&mut Registry<KV, M>) + Send;

/// Two possible commands to send: operation or exit.
enum Command<KV: KeyValueStore, M: Mode> {
    Operation(Box<DynCommand<KV, M>>),
    Exit,
}

/// Registry whose state is kept in a background thread.
pub struct BackgroundRegistry<KV: KeyValueStore, G, M: Mode> {
    cmd_sender: SyncSender<Command<KV, M>>,
    handle: Option<JoinHandle<()>>,
    _pd: PhantomData<G>,
}

impl<'a, KV, G> TryFrom<&'a Registry<KV, Normal>> for BackgroundRegistry<KV, G, Prove<'static>>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    G: GcNames,
{
    type Error = OperationalError;

    fn try_from(registry: &'a Registry<KV, Normal>) -> Result<Self, Self::Error> {
        // TODO (TZX-146): `try_start_proof` already clones internally, but we're
        //   required to clone here as whatever we capture in `spawn` must live
        //   for `'static`.
        let registry = registry.try_clone()?;

        let (init_sender, init_receiver) = sync_channel::<Result<(), Self::Error>>(0);
        let (cmd_sender, cmd_receiver) = sync_channel::<Command<KV, Prove<'static>>>(1);

        let handle = std::thread::spawn(move || {
            let mut prove = match registry.try_start_proof() {
                Ok(prove) => {
                    let Ok(()) = init_sender.send(Ok(())) else {
                        return;
                    };
                    prove
                }
                Err(err) => {
                    // if we fail to send, swallow the error as we exit anyway
                    let _ = init_sender.send(Err(err));
                    return;
                }
            };

            // blocking receive: the only error case is the channel is
            // closed - ie prove mode is dropped. Therefore
            // exit our end too on error.
            while let Ok(cmd) = cmd_receiver.recv() {
                match cmd {
                    Command::Operation(op) => op(&mut prove),
                    Command::Exit => return,
                }
            }
        });

        let () = init_receiver
            .recv()
            .map_err(|_| OperationalError::WorkerThreadDied)??;

        Ok(Self {
            cmd_sender,
            handle: Some(handle),
            _pd: PhantomData,
        })
    }
}

/// Error building a verify-mode [`BackgroundRegistry`] from a proof.
#[derive(Debug, thiserror::Error)]
pub enum BuildVerifyError {
    /// The worker thread could not be set up.
    #[error("Failed to initialise verification worker thread {0}")]
    Operational(#[from] OperationalError),

    /// The proof did not describe a valid registry.
    ///
    /// The error is converted into a string, as the error returned from the proof deserialisation
    /// is `!Send`, and so cannot be passed from the background thread.
    #[error("invalid durable-storage proof: {0}")]
    Proof(String),
}

impl<KV, G> BackgroundRegistry<KV, G, Verify>
where
    KV: BackgroundKeyValueStore,
    G: GcNames,
{
    /// Build a verify-mode registry that replays operations against `proof`.
    pub fn from_proof(proof: MerkleProof) -> Result<Self, BuildVerifyError> {
        let (init_sender, init_receiver) = sync_channel::<Result<(), String>>(0);
        let (cmd_sender, cmd_receiver) = sync_channel::<Command<KV, Verify>>(1);

        let handle = std::thread::spawn(move || {
            let mut verify = match Registry::<KV, Verify>::from_proof(ProofPart::Present(&proof)) {
                Ok(suspended) => {
                    let Ok(()) = init_sender.send(Ok(())) else {
                        return;
                    };
                    suspended.into_result()
                }
                Err(err) => {
                    // if we fail to send, swallow the error as we exit anyway
                    let _ = init_sender.send(Err(err.to_string()));
                    return;
                }
            };

            // blocking receive: the only error case is the channel being closed - ie the
            // verify handle is dropped. Exit our end too on error.
            while let Ok(cmd) = cmd_receiver.recv() {
                match cmd {
                    Command::Operation(op) => op(&mut verify),
                    Command::Exit => return,
                }
            }
        });

        match init_receiver.recv() {
            Ok(Ok(())) => (),
            Ok(Err(msg)) => return Err(BuildVerifyError::Proof(msg)),
            Err(_) => return Err(BuildVerifyError::from(OperationalError::WorkerThreadDied)),
        }

        Ok(Self {
            cmd_sender,
            handle: Some(handle),
            _pd: PhantomData,
        })
    }
}

/// Implement `Drop` - essentially don't drop the `BackgroundRegistry`
/// until the background thread has exited. While not strictly necessary,
/// this makes it more likely that the `BackgroundRegistry` has been dropped,
/// by the time the OCaml major GC finishes.
impl<KV, G, M> std::ops::Drop for BackgroundRegistry<KV, G, M>
where
    KV: KeyValueStore,
    M: Mode,
{
    fn drop(&mut self) {
        let Ok(()) = self.cmd_sender.send(Command::Exit) else {
            // already exited
            return;
        };
        let Some(handle) = self.handle.take() else {
            return;
        };

        // don't care if the background thread exited successfully,
        // only that it has exited.
        let _ = handle.join();
    }
}

impl<KV, G, M> ocaml::Custom for BackgroundRegistry<KV, G, M>
where
    KV: KeyValueStore,
    G: GcNames,
    M: Mode,
{
    const NAME: &'static str = G::MUTABLE_NAME;

    const OPS: ocaml::custom::CustomOps = ocaml::custom::CustomOps {
        identifier: Self::NAME.as_ptr() as *const ocaml::sys::Char,
        ..ocaml::custom::CustomOps {
            finalize: Some(Self::finalize),
            ..ocaml::custom::DEFAULT_CUSTOM_OPS
        }
    };

    const USED: usize = 1;
    const MAX: usize = 10;
}

/// Applies the given function over the registry state kept in the background
/// thread, returning the result.
// Kept as a macro - as that's the only way to really abstract over the 'mutablity' of the registry required by the command in question.
macro_rules! background_apply {
    ($self:ident, $fun:ident, $kv:ty, $m:ty) => {{
        let (res_sender, res_receiver) = sync_channel(1);
        let command = Box::new(move |registry: &mut Registry<$kv, $m>| {
            let res = $fun(registry);

            res_sender.send(res).expect("Sending result should succeed");
        });

        $self
            .cmd_sender
            .send(Command::Operation(command))
            .map_err(|_| OperationalError::WorkerThreadDied)?;

        res_receiver
            .recv()
            .map_err(|_| OperationalError::WorkerThreadDied)
    }};
}

impl<KV, G> RegistryApply<KV, Prove<'static>> for BackgroundRegistry<KV, G, Prove<'static>>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
{
    type NotFoundError = Infallible;

    fn apply<F, R>(&self, fun: F) -> Result<Result<R, Infallible>, OperationalError>
    where
        F: FnOnce(&mut Registry<KV, Prove<'static>>) -> R + Send + 'static,
        R: Send + 'static,
        KV::Repo: Clone,
    {
        Ok(Ok(background_apply!(self, fun, KV, Prove<'static>)?))
    }

    fn apply_ro<F, R>(&self, fun: F) -> Result<Result<R, Infallible>, OperationalError>
    where
        F: FnOnce(&Registry<KV, Prove<'static>>) -> R + Send + 'static,
        R: Send + 'static,
    {
        Ok(Ok(background_apply!(self, fun, KV, Prove<'static>)?))
    }
}

/// Like [`background_apply`], but runs the operation under [`catch_not_found`] in the worker
/// thread.
macro_rules! background_apply_verify {
    ($self:ident, $fun:ident, $kv:ty) => {{
        let fun = move |registry: &mut Registry<$kv, Verify>| {
            // `AssertUnwindSafe`: the registry is abandoned once an operation diverges, so a
            // mid-operation unwind leaving it inconsistent is harmless.
            //
            // TODO (TZX-174): `NotFound` errors should result in poisoning the verify
            //     state - any subsequent uses of the registry must fail - to enforce the
            //     above property.
            catch_not_found(AssertUnwindSafe(move || $fun(registry)))
        };

        background_apply!($self, fun, $kv, Verify)
    }};
}

impl<KV, G> RegistryApply<KV, Verify> for BackgroundRegistry<KV, G, Verify>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
{
    // Verify mode replays against a proof; a touch of absent data diverges with `NotFound`.
    type NotFoundError = NotFound;

    fn apply<F, R>(&self, fun: F) -> Result<Result<R, NotFound>, OperationalError>
    where
        F: FnOnce(&mut Registry<KV, Verify>) -> R + Send + 'static,
        R: Send + 'static,
        KV::Repo: Clone,
    {
        background_apply_verify!(self, fun, KV)
    }

    fn apply_ro<F, R>(&self, fun: F) -> Result<Result<R, NotFound>, OperationalError>
    where
        F: FnOnce(&Registry<KV, Verify>) -> R + Send + 'static,
        R: Send + 'static,
    {
        background_apply_verify!(self, fun, KV)
    }
}
