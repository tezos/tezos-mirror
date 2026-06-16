// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::path::RefPath;

#[derive(Debug, PartialEq, Eq)]
pub struct RefBytes<'a>(pub &'a [u8]);

impl AsRef<[u8]> for RefBytes<'_> {
    fn as_ref(&self) -> &[u8] {
        self.0
    }
}

#[allow(clippy::from_over_into)]
impl Into<[u8; PREIMAGE_HASH_SIZE]> for RefBytes<'_> {
    fn into(self) -> [u8; PREIMAGE_HASH_SIZE] {
        self.0.try_into().unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MoveInstruction<Path> {
    pub from: Path,
    pub to: Path,
}

// Value dependent instructions start here

#[derive(Debug, PartialEq, Eq)]
pub struct RevealInstruction<Path, Bytes> {
    pub hash: Bytes,
    pub to: Path,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SetInstruction<Path, Bytes> {
    pub value: Bytes,
    pub to: Path,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConfigInstruction<Path, Bytes> {
    Reveal(RevealInstruction<Path, Bytes>),
    Move(MoveInstruction<Path>),
    Set(SetInstruction<Path, Bytes>),
}

pub type RefConfigInstruction<'a> = ConfigInstruction<RefPath<'a>, RefBytes<'a>>;

#[cfg(feature = "alloc")]
pub mod owned {
    use crate::binary::evaluation::eval_config_instr;
    use tezos_smart_rollup_encoding::dac::PreimageHash;
    use tezos_smart_rollup_host::{
        path::{OwnedPath, PathError},
        reveal::HostReveal,
        storage::StorageV1,
    };
    use thiserror::Error;

    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub struct OwnedBytes(pub Vec<u8>);

    impl AsRef<[u8]> for OwnedBytes {
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }

    pub type OwnedConfigInstruction = ConfigInstruction<OwnedPath, OwnedBytes>;

    #[derive(Debug, PartialEq, Eq)]
    pub struct OwnedConfigProgram(pub Vec<OwnedConfigInstruction>);

    impl OwnedConfigProgram {
        pub fn evaluate<Host>(&self, host: &mut Host) -> Result<(), &'static str>
        where
            Host: StorageV1 + HostReveal,
        {
            for instruction in self.0.iter() {
                eval_config_instr(host, instruction)?
            }
            Ok(())
        }
    }

    #[derive(Debug, Error, PartialEq)]
    pub enum RevealInstrError {
        #[error("Invalid preimage hash size: {0}")]
        InvalidRevealHashSize(usize),
        #[error("Invalid reveal path: {0}")]
        PathError(PathError),
    }

    impl OwnedConfigInstruction {
        pub fn reveal_instr(hash: PreimageHash, to: OwnedPath) -> Self {
            OwnedConfigInstruction::Reveal(RevealInstruction {
                hash: OwnedBytes(hash.into()),
                to,
            })
        }

        pub fn move_instr(from: OwnedPath, to: OwnedPath) -> Self {
            OwnedConfigInstruction::Move(MoveInstruction { from, to })
        }

        pub fn set_instr(value: OwnedBytes, to: OwnedPath) -> Self {
            OwnedConfigInstruction::Set(SetInstruction { value, to })
        }
    }
}

pub mod evaluation {
    use crate::binary::instr::{MoveInstruction, RevealInstruction, SetInstruction};
    use crate::binary::reveal_root_hash_to_store;
    use tezos_smart_rollup_host::path::Path as HostPath;
    use tezos_smart_rollup_host::reveal::HostReveal;
    use tezos_smart_rollup_host::storage::StorageV1;

    use super::ConfigInstruction;

    /// Configuration instruction evaluation
    ///
    /// This functions takes a config instruction and evaluates it with the according
    /// intended behaviour.
    ///
    /// Here's an example of how you can use this function:
    /// ```
    /// use tezos_smart_rollup_installer_config::binary::evaluation::eval_config_instr;
    /// use tezos_smart_rollup_host::KERNEL_BOOT_PATH;
    /// use tezos_smart_rollup_host::path::OwnedPath;
    /// use tezos_smart_rollup_host::path::RefPath;
    /// use tezos_smart_rollup_installer_config::binary::owned::OwnedConfigInstruction;
    /// use tezos_smart_rollup_installer_config::binary::owned::OwnedConfigProgram;
    /// use tezos_smart_rollup_installer_config::binary::promote::upgrade_reveal_flow;
    /// use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    /// use tezos_smart_rollup_host::reveal::HostReveal;
    /// use tezos_smart_rollup_host::storage::StorageV1;
    ///
    ///
    /// pub fn upgrade_kernel<Host>(host: &mut Host) -> Result<(), &'static str>
    /// where
    ///     Host: HostReveal + StorageV1
    /// {
    ///     let root_hash = [0; PREIMAGE_HASH_SIZE];
    ///
    ///     // Create config consisting of a reveal instruction followed by a move.
    ///     let config = upgrade_reveal_flow(root_hash);
    ///
    ///     config.evaluate(host)
    /// }
    /// ```
    pub fn eval_config_instr<Host, Path: HostPath, Bytes: AsRef<[u8]>>(
        host: &mut Host,
        config_instr: &ConfigInstruction<Path, Bytes>,
    ) -> Result<(), &'static str>
    where
        Host: StorageV1 + HostReveal,
    {
        match config_instr {
            ConfigInstruction::Reveal(RevealInstruction { hash, to }) => {
                let hash = &hash
                    .as_ref()
                    .try_into()
                    .map_err(|_| "Invalid hash conversion.")?;
                reveal_root_hash_to_store(host, hash, to)
            }
            ConfigInstruction::Move(MoveInstruction { from, to }) => host
                .store_move(from, to)
                .map_err(|_| "Couldn't move path during config application"),
            ConfigInstruction::Set(SetInstruction { value, to }) => host
                .store_write(to, value.as_ref(), 0)
                .map_err(|_| "Couldn't set key during config application"),
        }
    }
}

#[cfg(feature = "alloc")]
pub mod promote {
    use super::owned::{OwnedConfigInstruction, OwnedConfigProgram};
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    use tezos_smart_rollup_host::{
        path::{OwnedPath, RefPath},
        KERNEL_BOOT_PATH,
    };

    const TMP_REVEAL_PATH: RefPath = RefPath::assert_from(b"/__sdk/installer/reveal");

    pub fn upgrade_reveal_flow(
        root_hash: [u8; PREIMAGE_HASH_SIZE],
    ) -> OwnedConfigProgram {
        OwnedConfigProgram(vec![
            OwnedConfigInstruction::reveal_instr(
                root_hash.to_vec().into(),
                OwnedPath::from(&TMP_REVEAL_PATH),
            ),
            OwnedConfigInstruction::move_instr(
                (&TMP_REVEAL_PATH).into(),
                (&KERNEL_BOOT_PATH).into(),
            ),
        ])
    }
}
