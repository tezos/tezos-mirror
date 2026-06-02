// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generic registry state wrapper for OCaml GC resource tracking.

use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::mpsc::SyncSender;
use std::sync::mpsc::sync_channel;
use std::thread::JoinHandle;

use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::mode::Mode;
use octez_riscv_data::mode::Normal;
use octez_riscv_data::mode::ProvableExt;
use octez_riscv_data::mode::Prove;
use octez_riscv_durable_storage::errors::OperationalError;
use octez_riscv_durable_storage::registry::Registry;
use octez_riscv_durable_storage::storage::KeyValueStore;

use crate::api_common::BackgroundKeyValueStore;
use crate::api_common::RegistryApply;

/// Type alias for a mutable registry state backed by a generic key-value store.
pub type DsRegistry<KV, G> = MutableState<RegistryState<KV, G>>;

/// Type alias for a prove-mode registry state backed by a generic key-value store.
pub type DsProveRegistry<KV, G> = BackgroundRegistry<KV, G, Prove<'static>>;

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
    fn apply<F, R>(&self, fun: F) -> Result<R, OperationalError>
    where
        F: FnOnce(&mut Registry<KV, Normal>) -> R + Send + 'static,
        KV::Repo: Clone,
    {
        MutableState::apply(self, fun)
    }

    fn apply_ro<F, R>(&self, fun: F) -> Result<R, OperationalError>
    where
        F: FnOnce(&Registry<KV, Normal>) -> R + Send + 'static,
    {
        Ok(MutableState::apply_ro(self, fun))
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

impl<KV, G, M> RegistryApply<KV, M> for BackgroundRegistry<KV, G, M>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
    M: Mode,
{
    fn apply<F, R>(&self, fun: F) -> Result<R, OperationalError>
    where
        F: FnOnce(&mut Registry<KV, M>) -> R + Send + 'static,
        R: Send + 'static,
        KV::Repo: Clone,
    {
        background_apply!(self, fun, KV, M)
    }

    fn apply_ro<F, R>(&self, fun: F) -> Result<R, OperationalError>
    where
        F: FnOnce(&Registry<KV, M>) -> R + Send + 'static,
        R: Send + 'static,
    {
        background_apply!(self, fun, KV, M)
    }
}
