#![allow(dead_code)]
//! Create a standalone native executable for a given Wasm file.

use crate::store::CompilerOptions;
use anyhow::{Context, Result};
use clap::Parser;
use std::env;

use std::path::PathBuf;

use wasmer::*;

#[derive(Debug, Parser)]
/// The options for the `wasmer create-exe` subcommand
pub struct CreateObj {
    /// Input file
    #[clap(name = "FILE", parse(from_os_str))]
    path: PathBuf,

    /// Output file or directory if the input is a pirita file
    #[clap(name = "OUTPUT_PATH", short = 'o', parse(from_os_str))]
    output: PathBuf,

    /// Optional directorey used for debugging: if present, will
    /// output the files to a debug instead of a temp directory
    #[clap(long, name = "DEBUG PATH", parse(from_os_str))]
    debug_dir: Option<PathBuf>,

    /// Prefix for the function names in the input file in the compiled object file.
    ///
    /// Default value = sha256 of the input file
    #[clap(long, name = "PREFIX")]
    prefix: Option<String>,

    /// Atom name to compile when compiling multi-atom pirita files
    #[clap(long, name = "ATOM")]
    atom: Option<String>,

    /// Compilation Target triple
    ///
    /// Accepted target triple values must follow the
    /// ['target_lexicon'](https://crates.io/crates/target-lexicon) crate format.
    ///
    /// The recommended targets we try to support are:
    ///
    /// - "x86_64-linux-gnu"
    /// - "aarch64-linux-gnu"
    /// - "x86_64-apple-darwin"
    /// - "arm64-apple-darwin"
    /// - "x86_64-windows-gnu"
    #[clap(long = "target")]
    target_triple: Option<Triple>,

    #[clap(long, short = 'm', multiple = true, number_of_values = 1)]
    cpu_features: Vec<CpuFeature>,

    #[clap(flatten)]
    compiler: CompilerOptions,
}

impl CreateObj {
    /// Runs logic for the `create-obj` subcommand
    pub fn execute(&self) -> Result<()> {
        let path = crate::common::normalize_path(&format!("{}", self.path.display()));
        let target_triple = self.target_triple.clone().unwrap_or_else(Triple::host);
        let starting_cd = env::current_dir()?;
        let input_path = starting_cd.join(&path);
        let temp_dir = tempfile::tempdir();
        let output_directory_path = match self.debug_dir.as_ref() {
            Some(s) => s.clone(),
            None => temp_dir?.path().to_path_buf(),
        };
        std::fs::create_dir_all(&output_directory_path)?;
        let prefix = match self.prefix.as_ref() {
            Some(s) => vec![s.clone()],
            None => Vec::new(),
        };

        let target = crate::commands::create_exe::utils::target_triple_to_target(
            &target_triple,
            &self.cpu_features,
        );
        let (_, compiler_type) = self.compiler.get_store_for_target(target.clone())?;
        println!("Compiler: {}", compiler_type.to_string());
        println!("Target: {}", target.triple());

        let atoms = if let Ok(pirita) =
            webc::v1::WebCMmap::parse(input_path.clone(), &webc::v1::ParseOptions::default())
        {
            crate::commands::create_exe::compile_pirita_into_directory(
                &pirita,
                &output_directory_path,
                &self.compiler,
                &self.cpu_features,
                &target_triple,
                &prefix,
                crate::commands::AllowMultiWasm::Reject(self.atom.clone()),
                self.debug_dir.is_some(),
            )
        } else {
            crate::commands::create_exe::prepare_directory_from_single_wasm_file(
                &input_path,
                &output_directory_path,
                &self.compiler,
                &target_triple,
                &self.cpu_features,
                &prefix,
                self.debug_dir.is_some(),
            )
        }?;

        // Copy output files into target path, depending on whether
        // there are one or many files being compiled
        let file_paths = std::fs::read_dir(output_directory_path.join("atoms"))
            .map_err(|e| {
                anyhow::anyhow!(
                    "could not read {}: {e}",
                    output_directory_path.join("atoms").display()
                )
            })?
            .filter_map(|path| path.ok()?.path().canonicalize().ok())
            .collect::<Vec<_>>();

        if file_paths.is_empty() {
            return Err(anyhow::anyhow!(
                "could not compile object file: no output objects in {}",
                output_directory_path.join("atoms").display()
            ));
        }

        if file_paths.len() == 1 {
            if let Some(parent) = self.output.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::copy(
                std::env::current_dir().unwrap().join(&file_paths[0]),
                std::env::current_dir().unwrap().join(&self.output),
            )
            .map_err(|e| {
                anyhow::anyhow!(
                    "{} -> {}: {e}",
                    &file_paths[0].display(),
                    self.output.display()
                )
            })?;
        } else {
            let keys = atoms
                .iter()
                .map(|(name, _)| name.clone())
                .collect::<Vec<_>>();
            return Err(anyhow::anyhow!(
                "where <ATOM> is one of: {}",
                keys.join(", ")
            ))
            .context(anyhow::anyhow!(
                "note: use --atom <ATOM> to specify which atom to compile"
            ))
            .context(anyhow::anyhow!(
                "cannot compile more than one atom at a time"
            ));
        }

        let output_file = self.output.canonicalize().unwrap().display().to_string();
        let output_file = output_file
            .strip_prefix(r"\\?\")
            .unwrap_or(&output_file)
            .to_string();

        eprintln!("✔ Object compiled successfully to `{output_file}`");

        Ok(())
    }
}
