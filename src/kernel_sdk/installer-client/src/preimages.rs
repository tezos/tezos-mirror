// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fs;
use std::path::Path;
use tezos_smart_rollup_encoding::dac::prepare_preimages;
use tezos_smart_rollup_encoding::dac::PreimageHash;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unable to read content file: {0}.")]
    ContentFile(std::io::Error),
    #[error("Unable to create preimages dir: {0}.")]
    PreimagesDir(std::io::Error),
    #[error("Failed to produce preimages from content: {0}.")]
    Preimage(String),
}

pub fn content_to_preimages(
    content: &Path,
    preimage_dir: &Path,
) -> Result<PreimageHash, Error> {
    if !preimage_dir.is_dir() {
        fs::create_dir_all(preimage_dir).map_err(Error::PreimagesDir)?;
    }

    let content = fs::read(content).map_err(Error::ContentFile)?;

    let save_preimages = |hash: PreimageHash, preimage: Vec<u8>| {
        let name = hex::encode(hash.as_ref());
        let path = preimage_dir.join(name);

        if let Err(e) = fs::write(&path, preimage) {
            eprintln!("Failed to write preimage to {:?} due to {}.", path, e);
        }
    };

    prepare_preimages(&content, save_preimages)
        .map_err(|e| Error::Preimage(e.to_string()))
}
