// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! All hosts provide a reveal capability: the ability to retrieve certain kinds of data,
//! without it needing to be posted through the L1 inbox.

use tezos_smart_rollup_core::{SmartRollupCore, PREIMAGE_HASH_SIZE};

use crate::{
    dal_parameters::RollupDalParameters, metadata::RollupMetadata, runtime::RuntimeError,
    Error, DAL_PARAMETERS_SIZE, METADATA_SIZE,
};

/// Core capability required to expose the SDK's reveal functionality for a given host.
pub trait HostReveal {
    /// Returns [RollupMetadata]
    fn reveal_metadata(&self) -> RollupMetadata;

    /// Reveal pre-image from a hash of size `PREIMAGE_HASH_SIZE` in bytes.
    ///
    /// N.B. in future, multiple hashing schemes will be supported, but for
    /// now the kernels only support hashes of type `Reveal_hash`, which is
    /// a 32-byte Blake2b hash with a prefix-byte of `0`.
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError>;

    /// Reveal the DAL parameters.
    fn reveal_dal_parameters(&self) -> RollupDalParameters;

    /// Reveal a DAL page.
    #[cfg(feature = "alloc")]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError>;
}

impl<Host: SmartRollupCore> HostReveal for Host {
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let res = unsafe {
            SmartRollupCore::reveal_preimage(
                self,
                hash.as_ptr(),
                PREIMAGE_HASH_SIZE,
                buffer.as_mut_ptr(),
                buffer.len(),
            )
        };
        match Error::wrap(res) {
            Ok(size) => Ok(size),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn reveal_metadata(&self) -> RollupMetadata {
        let mut destination = [0u8; METADATA_SIZE];
        let res = unsafe {
            SmartRollupCore::reveal_metadata(
                self,
                destination.as_mut_ptr(),
                destination.len(),
            )
        };

        // Revealing metadata should always succeed
        debug_assert!(res == METADATA_SIZE as i32, "SDK_ERROR: Revealing metadata always succeeds. \
                                             If you see this message, please report it to the \
                                             SDK developers at https://gitlab.com/tezos/tezos");

        RollupMetadata::from(destination)
    }

    #[cfg(feature = "alloc")]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        // This will match the encoding declared for a DAL page in the Tezos protocol.
        let payload: &[u8] = &[
            &[2u8], // tag
            published_level.to_be_bytes().as_ref(),
            &[slot_index],
            page_index.to_be_bytes().as_ref(),
        ]
        .concat();

        let res = unsafe {
            SmartRollupCore::reveal(
                self,
                payload.as_ptr(),
                payload.len(),
                destination.as_mut_ptr(),
                destination.len(),
            )
        };

        match Error::wrap(res) {
            Ok(size) => Ok(size),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        let mut destination = [0u8; DAL_PARAMETERS_SIZE];
        // This will match the encoding declared for revealing DAL parameters in the Tezos protocol.
        let payload: &[u8] = &[3u8]; // tag

        let bytes_read = unsafe {
            SmartRollupCore::reveal(
                self,
                payload.as_ptr(),
                payload.len(),
                destination.as_mut_ptr(),
                destination.len(),
            )
        };

        debug_assert!(bytes_read == DAL_PARAMETERS_SIZE as i32, "SDK_ERROR: Revealing DAL parameters should always succeed. \
                                             If you see this message, please report it to the \
                                             SDK developers at https://gitlab.com/tezos/tezos");

        match RollupDalParameters::try_from(destination) {
            Ok(dal_parameters) => dal_parameters,
            Err(_) => {
                debug_assert!(
                    false,
                    "SDK_ERROR: Decoding DAL parameters should always succeed. \
                     If you see this message, please report it to the \
                     SDK developers at https://gitlab.com/tezos/tezos"
                );
                unreachable!()
            }
        }
    }
}
