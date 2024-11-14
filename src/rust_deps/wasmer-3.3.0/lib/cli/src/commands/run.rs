use crate::common::get_cache_dir;
use crate::logging;
use crate::package_source::PackageSource;
use crate::store::{CompilerType, StoreOptions};
use crate::suggestions::suggest_function_exports;
use crate::warning;
use anyhow::{anyhow, Context, Result};
use clap::Parser;
#[cfg(feature = "coredump")]
use std::fs::File;
use std::io::Write;
use std::net::SocketAddr;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;
use wasmer::FunctionEnv;
use wasmer::*;
use wasmer_cache::{Cache, FileSystemCache, Hash};
use wasmer_types::Type as ValueType;
use wasmer_wasix::runners::Runner;

mod wasi;

pub(crate) use wasi::Wasi;

/// The options for the `wasmer run` subcommand, runs either a package, URL or a file
#[derive(Debug, Parser, Clone)]
pub struct Run {
    /// File to run
    #[clap(name = "SOURCE", parse(try_from_str))]
    pub(crate) path: PackageSource,
    /// Options to run the file / package / URL with
    #[clap(flatten)]
    pub(crate) options: RunWithoutFile,
}

/// Same as `wasmer run`, but without the required `path` argument (injected previously)
#[derive(Debug, Parser, Clone, Default)]
pub struct RunWithoutFile {
    /// When installing packages with `wasmer $package`, force re-downloading the package
    #[clap(long = "force", short = 'f')]
    pub(crate) force_install: bool,

    /// Disable the cache
    #[clap(long = "disable-cache")]
    pub(crate) disable_cache: bool,

    /// Invoke a specified function
    #[clap(long = "invoke", short = 'i')]
    pub(crate) invoke: Option<String>,

    /// The command name is a string that will override the first argument passed
    /// to the wasm program. This is used in wapm to provide nicer output in
    /// help commands and error messages of the running wasm program
    #[clap(long = "command-name", hide = true)]
    pub(crate) command_name: Option<String>,

    /// A prehashed string, used to speed up start times by avoiding hashing the
    /// wasm module. If the specified hash is not found, Wasmer will hash the module
    /// as if no `cache-key` argument was passed.
    #[clap(long = "cache-key", hide = true)]
    pub(crate) cache_key: Option<String>,

    #[clap(flatten)]
    pub(crate) store: StoreOptions,

    // TODO: refactor WASI structure to allow shared options with Emscripten
    #[clap(flatten)]
    pub(crate) wasi: Wasi,

    /// Enable non-standard experimental IO devices
    #[cfg(feature = "io-devices")]
    #[clap(long = "enable-io-devices")]
    pub(crate) enable_experimental_io_devices: bool,

    /// Enable debug output
    #[clap(long = "debug", short = 'd')]
    pub(crate) debug: bool,

    #[clap(long = "verbose")]
    pub(crate) verbose: Option<u8>,

    #[clap(flatten)]
    pub(crate) wcgi: WcgiOptions,

    /// Enable coredump generation after a WebAssembly trap.
    #[clap(name = "COREDUMP PATH", long = "coredump-on-trap", parse(from_os_str))]
    coredump_on_trap: Option<PathBuf>,

    #[cfg(feature = "sys")]
    /// The stack size (default is 1048576)
    #[clap(long = "stack-size")]
    pub(crate) stack_size: Option<usize>,

    /// Application arguments
    #[clap(value_name = "ARGS")]
    pub(crate) args: Vec<String>,
}

/// Same as `Run`, but uses a resolved local file path.
#[derive(Debug, Clone, Default)]
pub struct RunWithPathBuf {
    /// File to run
    pub(crate) path: PathBuf,
    /// Options for running the file
    pub(crate) options: RunWithoutFile,
}

impl Deref for RunWithPathBuf {
    type Target = RunWithoutFile;
    fn deref(&self) -> &Self::Target {
        &self.options
    }
}

impl RunWithPathBuf {
    /// Execute the run command
    pub fn execute(&self) -> Result<()> {
        let mut self_clone = self.clone();

        if self_clone.path.is_dir() {
            let (manifest, pathbuf) = wasmer_registry::get_executable_file_from_path(
                &self_clone.path,
                self_clone.command_name.as_deref(),
            )?;

            // See https://github.com/wasmerio/wasmer/issues/3492 -
            // we need IndexMap to have a stable ordering for the [fs] mapping,
            // otherwise overlapping filesystem mappings might not work
            // since we want to control the order of mounting directories from the
            // wasmer.toml file
            let default = indexmap::IndexMap::default();
            let fs = manifest.fs.as_ref().unwrap_or(&default);
            for (alias, real_dir) in fs.iter() {
                let real_dir = self_clone.path.join(real_dir);
                if !real_dir.exists() {
                    #[cfg(feature = "debug")]
                    if self_clone.debug {
                        println!(
                            "warning: cannot map {alias:?} to {}: directory does not exist",
                            real_dir.display()
                        );
                    }
                    continue;
                }

                self_clone.options.wasi.map_dir(alias, real_dir.clone());
            }

            self_clone.path = pathbuf;
        }

        if self.debug {
            let level = match self_clone.verbose {
                Some(1) => log::LevelFilter::Debug,
                Some(_) | None => log::LevelFilter::Trace,
            };
            logging::set_up_logging(level);
        }

        let invoke_res = self_clone.inner_execute().with_context(|| {
            format!(
                "failed to run `{}`{}",
                self_clone.path.display(),
                if CompilerType::enabled().is_empty() {
                    " (no compilers enabled)"
                } else {
                    ""
                }
            )
        });

        if let Err(err) = invoke_res {
            #[cfg(feature = "coredump")]
            if let Some(coredump_path) = self.coredump_on_trap.as_ref() {
                let source_name = self.path.to_str().unwrap_or("unknown");
                if let Err(coredump_err) = generate_coredump(&err, source_name, coredump_path) {
                    eprintln!("warning: coredump failed to generate: {}", coredump_err);
                    Err(err)
                } else {
                    Err(err.context(format!("core dumped at {}", coredump_path.display())))
                }
            } else {
                Err(err)
            }

            #[cfg(not(feature = "coredump"))]
            Err(err)
        } else {
            invoke_res
        }
    }

    fn inner_module_run(&self, store: &mut Store, instance: Instance) -> Result<i32> {
        #[cfg(feature = "sys")]
        if self.stack_size.is_some() {
            wasmer_vm::set_stack_size(self.stack_size.unwrap());
        }
        // If this module exports an _initialize function, run that first.
        if let Ok(initialize) = instance.exports.get_function("_initialize") {
            initialize
                .call(store, &[])
                .with_context(|| "failed to run _initialize function")?;
        }

        // Do we want to invoke a function?
        if let Some(ref invoke) = self.invoke {
            let result = self.invoke_function(store, &instance, invoke, &self.args)?;
            println!(
                "{}",
                result
                    .iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            );
        } else {
            let start: Function = self.try_find_function(&instance, "_start", &[])?;
            let result = start.call(store, &[]);
            #[cfg(feature = "wasi")]
            return self.wasi.handle_result(result);
            #[cfg(not(feature = "wasi"))]
            return Ok(result?);
        }

        Ok(0)
    }

    fn inner_execute(&self) -> Result<()> {
        #[cfg(feature = "webc_runner")]
        {
            if let Ok(pf) = webc::Container::from_disk(self.path.clone()) {
                return self.run_container(pf, self.command_name.as_deref(), &self.args);
            }
        }
        let (mut store, module) = self.get_store_module()?;
        #[cfg(feature = "emscripten")]
        {
            use wasmer_emscripten::{
                generate_emscripten_env, is_emscripten_module, run_emscripten_instance, EmEnv,
                EmscriptenGlobals,
            };
            // TODO: refactor this
            if is_emscripten_module(&module) {
                let em_env = EmEnv::new();
                for (k, v) in self.wasi.env_vars.iter() {
                    em_env.set_env_var(k, v);
                }
                // create an EmEnv with default global
                let env = FunctionEnv::new(&mut store, em_env);
                let mut emscripten_globals = EmscriptenGlobals::new(&mut store, &env, &module)
                    .map_err(|e| anyhow!("{}", e))?;
                env.as_mut(&mut store).set_data(
                    &emscripten_globals.data,
                    self.wasi
                        .mapped_dirs
                        .clone()
                        .into_iter()
                        .map(|d| (d.guest, d.host))
                        .collect(),
                );
                let import_object =
                    generate_emscripten_env(&mut store, &env, &mut emscripten_globals);
                let mut instance = match Instance::new(&mut store, &module, &import_object) {
                    Ok(instance) => instance,
                    Err(e) => {
                        let err: Result<(), _> = Err(e);
                        #[cfg(feature = "wasi")]
                        {
                            if Wasi::has_wasi_imports(&module) {
                                return err.with_context(|| "This module has both Emscripten and WASI imports. Wasmer does not currently support Emscripten modules using WASI imports.");
                            }
                        }
                        return err.with_context(|| "Can't instantiate emscripten module");
                    }
                };

                run_emscripten_instance(
                    &mut instance,
                    env.into_mut(&mut store),
                    &mut emscripten_globals,
                    if let Some(cn) = &self.command_name {
                        cn
                    } else {
                        self.path.to_str().unwrap()
                    },
                    self.args.iter().map(|arg| arg.as_str()).collect(),
                    self.invoke.clone(),
                )?;
                return Ok(());
            }
        }

        // If WASI is enabled, try to execute it with it
        #[cfg(feature = "wasi")]
        let ret = {
            use std::collections::BTreeSet;
            use wasmer_wasix::WasiVersion;

            let wasi_versions = Wasi::get_versions(&module);
            match wasi_versions {
                Some(wasi_versions) if !wasi_versions.is_empty() => {
                    if wasi_versions.len() >= 2 {
                        let get_version_list = |versions: &BTreeSet<WasiVersion>| -> String {
                            versions
                                .iter()
                                .map(|v| format!("`{}`", v.get_namespace_str()))
                                .collect::<Vec<String>>()
                                .join(", ")
                        };
                        if self.wasi.deny_multiple_wasi_versions {
                            let version_list = get_version_list(&wasi_versions);
                            bail!("Found more than 1 WASI version in this module ({}) and `--deny-multiple-wasi-versions` is enabled.", version_list);
                        } else if !self.wasi.allow_multiple_wasi_versions {
                            let version_list = get_version_list(&wasi_versions);
                            warning!("Found more than 1 WASI version in this module ({}). If this is intentional, pass `--allow-multiple-wasi-versions` to suppress this warning.", version_list);
                        }
                    }

                    let program_name = self
                        .command_name
                        .clone()
                        .or_else(|| {
                            self.path
                                .file_name()
                                .map(|f| f.to_string_lossy().to_string())
                        })
                        .unwrap_or_default();
                    let (ctx, instance) = self
                        .wasi
                        .instantiate(&mut store, &module, program_name, self.args.clone())
                        .with_context(|| "failed to instantiate WASI module")?;
                    let res = self.inner_module_run(&mut store, instance);

                    ctx.cleanup(&mut store, None);

                    res
                }
                // not WASI
                _ => {
                    let instance = Instance::new(&mut store, &module, &imports! {})?;
                    self.inner_module_run(&mut store, instance)
                }
            }
        }.map(|exit_code| {
            std::io::stdout().flush().ok();
            std::io::stderr().flush().ok();
            std::process::exit(exit_code);
        });

        #[cfg(not(feature = "wasi"))]
        let ret = {
            let instance = Instance::new(&module, &imports! {})?;

            // If this module exports an _initialize function, run that first.
            if let Ok(initialize) = instance.exports.get_function("_initialize") {
                initialize
                    .call(&[])
                    .with_context(|| "failed to run _initialize function")?;
            }

            // Do we want to invoke a function?
            if let Some(ref invoke) = self.invoke {
                let result = self.invoke_function(&instance, invoke, &self.args)?;
                println!(
                    "{}",
                    result
                        .iter()
                        .map(|val| val.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                );
            } else {
                let start: Function = self.try_find_function(&instance, "_start", &[])?;
                let result = start.call(&[]);
                #[cfg(feature = "wasi")]
                self.wasi.handle_result(result)?;
                #[cfg(not(feature = "wasi"))]
                result?;
            }
        };

        ret
    }

    #[cfg(feature = "webc_runner")]
    fn run_container(
        &self,
        container: webc::Container,
        id: Option<&str>,
        args: &[String],
    ) -> Result<(), anyhow::Error> {
        let id = id
            .or_else(|| container.manifest().entrypoint.as_deref())
            .context("No command specified")?;
        let command = container
            .manifest()
            .commands
            .get(id)
            .with_context(|| format!("No metadata found for the command, \"{id}\""))?;

        let (store, _compiler_type) = self.store.get_store()?;
        let mut runner = wasmer_wasix::runners::wasi::WasiRunner::new(store);
        runner.set_args(args.to_vec());
        if runner.can_run_command(id, command).unwrap_or(false) {
            return runner.run_cmd(&container, id).context("WASI runner failed");
        }

        let (store, _compiler_type) = self.store.get_store()?;
        let mut runner = wasmer_wasix::runners::emscripten::EmscriptenRunner::new(store);
        runner.set_args(args.to_vec());
        if runner.can_run_command(id, command).unwrap_or(false) {
            return runner
                .run_cmd(&container, id)
                .context("Emscripten runner failed");
        }

        let mut runner = wasmer_wasix::runners::wcgi::WcgiRunner::new(id);
        let (store, _compiler_type) = self.store.get_store()?;
        runner
            .config()
            .args(args)
            .store(store)
            .addr(self.wcgi.addr)
            .envs(self.wasi.env_vars.clone())
            .map_directories(self.wasi.mapped_dirs.clone());
        if self.wasi.forward_host_env {
            runner.config().forward_host_env();
        }
        if runner.can_run_command(id, command).unwrap_or(false) {
            return runner.run_cmd(&container, id).context("WCGI runner failed");
        }

        anyhow::bail!(
            "Unable to find a runner that supports \"{}\"",
            command.runner
        );
    }

    fn get_store_module(&self) -> Result<(Store, Module)> {
        let contents = std::fs::read(self.path.clone())?;
        #[cfg(not(feature = "jsc"))]
        if wasmer_compiler::Artifact::is_deserializable(&contents) {
            let engine = wasmer_compiler::EngineBuilder::headless();
            let store = Store::new(engine);
            let module = Module::deserialize_from_file_checked(&store, &self.path)?;
            return Ok((store, module));
        }
        let (store, compiler_type) = self.store.get_store()?;
        #[cfg(feature = "cache")]
        let module_result: Result<Module> = if !self.disable_cache && contents.len() > 0x1000 {
            self.get_module_from_cache(&store, &contents, &compiler_type)
        } else {
            Module::new(&store, contents).map_err(|e| e.into())
        };
        #[cfg(not(feature = "cache"))]
        let module_result = Module::new(&store, &contents);

        let mut module = module_result.with_context(|| {
            format!(
                "module instantiation failed (compiler: {})",
                compiler_type.to_string()
            )
        })?;
        // We set the name outside the cache, to make sure we dont cache the name
        module.set_name(&self.path.file_name().unwrap_or_default().to_string_lossy());

        Ok((store, module))
    }

    #[cfg(feature = "cache")]
    fn get_module_from_cache(
        &self,
        store: &Store,
        contents: &[u8],
        compiler_type: &CompilerType,
    ) -> Result<Module> {
        // We try to get it from cache, in case caching is enabled
        // and the file length is greater than 4KB.
        // For files smaller than 4KB caching is not worth,
        // as it takes space and the speedup is minimal.
        let mut cache = self.get_cache(compiler_type)?;
        // Try to get the hash from the provided `--cache-key`, otherwise
        // generate one from the provided file `.wasm` contents.
        let hash = self
            .cache_key
            .as_ref()
            .and_then(|key| Hash::from_str(key).ok())
            .unwrap_or_else(|| Hash::generate(contents));
        match unsafe { cache.load(store, hash) } {
            Ok(module) => Ok(module),
            Err(e) => {
                match e {
                    DeserializeError::Io(_) => {
                        // Do not notify on IO errors
                    }
                    err => {
                        warning!("cached module is corrupted: {}", err);
                    }
                }
                let module = Module::new(store, contents)?;
                // Store the compiled Module in cache
                cache.store(hash, &module)?;
                Ok(module)
            }
        }
    }

    #[cfg(feature = "cache")]
    /// Get the Compiler Filesystem cache
    fn get_cache(&self, compiler_type: &CompilerType) -> Result<FileSystemCache> {
        let mut cache_dir_root = get_cache_dir();
        cache_dir_root.push(compiler_type.to_string());
        let mut cache = FileSystemCache::new(cache_dir_root)?;

        let extension = "wasmu";
        cache.set_cache_extension(Some(extension));
        Ok(cache)
    }

    fn try_find_function(
        &self,
        instance: &Instance,
        name: &str,
        args: &[String],
    ) -> Result<Function> {
        Ok(instance
            .exports
            .get_function(name)
            .map_err(|e| {
                if instance.module().info().functions.is_empty() {
                    anyhow!("The module has no exported functions to call.")
                } else {
                    let suggested_functions = suggest_function_exports(instance.module(), "");
                    let names = suggested_functions
                        .iter()
                        .take(3)
                        .map(|arg| format!("`{}`", arg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let suggested_command = format!(
                        "wasmer {} -i {} {}",
                        self.path.display(),
                        suggested_functions.get(0).unwrap_or(&String::new()),
                        args.join(" ")
                    );
                    let suggestion = if suggested_functions.is_empty() {
                        String::from("Can not find any export functions.")
                    } else {
                        format!(
                            "Similar functions found: {}.\nTry with: {}",
                            names, suggested_command
                        )
                    };
                    match e {
                        ExportError::Missing(_) => {
                            anyhow!("No export `{}` found in the module.\n{}", name, suggestion)
                        }
                        ExportError::IncompatibleType => anyhow!(
                            "Export `{}` found, but is not a function.\n{}",
                            name,
                            suggestion
                        ),
                    }
                }
            })?
            .clone())
    }

    fn invoke_function(
        &self,
        ctx: &mut impl AsStoreMut,
        instance: &Instance,
        invoke: &str,
        args: &[String],
    ) -> Result<Box<[Value]>> {
        let func: Function = self.try_find_function(instance, invoke, args)?;
        let func_ty = func.ty(ctx);
        let required_arguments = func_ty.params().len();
        let provided_arguments = args.len();
        if required_arguments != provided_arguments {
            bail!(
                "Function expected {} arguments, but received {}: \"{}\"",
                required_arguments,
                provided_arguments,
                self.args.join(" ")
            );
        }
        let invoke_args = args
            .iter()
            .zip(func_ty.params().iter())
            .map(|(arg, param_type)| match param_type {
                ValueType::I32 => {
                    Ok(Value::I32(arg.parse().map_err(|_| {
                        anyhow!("Can't convert `{}` into a i32", arg)
                    })?))
                }
                ValueType::I64 => {
                    Ok(Value::I64(arg.parse().map_err(|_| {
                        anyhow!("Can't convert `{}` into a i64", arg)
                    })?))
                }
                ValueType::F32 => {
                    Ok(Value::F32(arg.parse().map_err(|_| {
                        anyhow!("Can't convert `{}` into a f32", arg)
                    })?))
                }
                ValueType::F64 => {
                    Ok(Value::F64(arg.parse().map_err(|_| {
                        anyhow!("Can't convert `{}` into a f64", arg)
                    })?))
                }
                _ => Err(anyhow!(
                    "Don't know how to convert {} into {:?}",
                    arg,
                    param_type
                )),
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(func.call(ctx, &invoke_args)?)
    }
}

impl Run {
    /// Executes the `wasmer run` command
    pub fn execute(&self) -> Result<(), anyhow::Error> {
        // downloads and installs the package if necessary
        let path_to_run = self.path.download_and_get_filepath()?;
        RunWithPathBuf {
            path: path_to_run,
            options: self.options.clone(),
        }
        .execute()
    }

    /// Create Run instance for arguments/env,
    /// assuming we're being run from a CFP binfmt interpreter.
    pub fn from_binfmt_args() -> Run {
        Self::from_binfmt_args_fallible().unwrap_or_else(|e| {
            crate::error::PrettyError::report::<()>(
                Err(e).context("Failed to set up wasmer binfmt invocation"),
            )
        })
    }

    #[cfg(target_os = "linux")]
    fn from_binfmt_args_fallible() -> Result<Run> {
        let argv = std::env::args().collect::<Vec<_>>();
        let (_interpreter, executable, original_executable, args) = match &argv[..] {
            [a, b, c, d @ ..] => (a, b, c, d),
            _ => {
                bail!("Wasmer binfmt interpreter needs at least three arguments (including $0) - must be registered as binfmt interpreter with the CFP flags. (Got arguments: {:?})", argv);
            }
        };
        let store = StoreOptions::default();
        // TODO: store.compiler.features.all = true; ?
        Ok(Self {
            // unwrap is safe, since parsing never fails
            path: PackageSource::parse(executable).unwrap(),
            options: RunWithoutFile {
                args: args.to_vec(),
                command_name: Some(original_executable.to_string()),
                store,
                wasi: Wasi::for_binfmt_interpreter()?,
                ..Default::default()
            },
        })
    }

    #[cfg(not(target_os = "linux"))]
    fn from_binfmt_args_fallible() -> Result<Run> {
        bail!("binfmt_misc is only available on linux.")
    }
}

#[cfg(feature = "coredump")]
fn generate_coredump(
    err: &anyhow::Error,
    source_name: &str,
    coredump_path: &PathBuf,
) -> Result<()> {
    let err = err
        .downcast_ref::<wasmer::RuntimeError>()
        .ok_or_else(|| anyhow!("no runtime error found to generate coredump with"))?;

    let mut coredump_builder =
        wasm_coredump_builder::CoredumpBuilder::new().executable_name(source_name);

    {
        let mut thread_builder = wasm_coredump_builder::ThreadBuilder::new().thread_name("main");

        for frame in err.trace() {
            let coredump_frame = wasm_coredump_builder::FrameBuilder::new()
                .codeoffset(frame.func_offset() as u32)
                .funcidx(frame.func_index())
                .build();
            thread_builder.add_frame(coredump_frame);
        }

        coredump_builder.add_thread(thread_builder.build());
    }

    let coredump = coredump_builder
        .serialize()
        .map_err(|err| anyhow!("failed to serialize coredump: {}", err))?;

    let mut f = File::create(coredump_path).context(format!(
        "failed to create file at `{}`",
        coredump_path.display()
    ))?;
    f.write_all(&coredump).with_context(|| {
        format!(
            "failed to write coredump file at `{}`",
            coredump_path.display()
        )
    })?;

    Ok(())
}

#[derive(Debug, Clone, Parser)]
pub(crate) struct WcgiOptions {
    /// The address to serve on.
    #[clap(long, short, env, default_value_t = ([127, 0, 0, 1], 8000).into())]
    pub(crate) addr: SocketAddr,
}

impl Default for WcgiOptions {
    fn default() -> Self {
        Self {
            addr: ([127, 0, 0, 1], 8000).into(),
        }
    }
}
