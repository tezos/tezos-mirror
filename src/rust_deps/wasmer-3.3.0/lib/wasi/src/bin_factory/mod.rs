use std::{
    collections::HashMap,
    ops::Deref,
    path::Path,
    sync::{Arc, RwLock},
};

use anyhow::Context;
use virtual_fs::{AsyncReadExt, FileSystem};

mod binary_package;
mod exec;
mod module_cache;

use sha2::*;

pub use self::{
    binary_package::*,
    exec::{spawn_exec, spawn_exec_module},
    module_cache::ModuleCache,
};
use crate::{os::command::Commands, WasiRuntime};

#[derive(Debug, Clone)]
pub struct BinFactory {
    pub(crate) commands: Commands,
    runtime: Arc<dyn WasiRuntime + Send + Sync + 'static>,
    pub(crate) cache: Arc<ModuleCache>,
    pub(crate) local: Arc<RwLock<HashMap<String, Option<BinaryPackage>>>>,
}

impl BinFactory {
    pub fn new(
        compiled_modules: Arc<ModuleCache>,
        runtime: Arc<dyn WasiRuntime + Send + Sync + 'static>,
    ) -> BinFactory {
        BinFactory {
            commands: Commands::new_with_builtins(runtime.clone(), compiled_modules.clone()),
            runtime,
            cache: compiled_modules,
            local: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn runtime(&self) -> &dyn WasiRuntime {
        self.runtime.deref()
    }

    pub fn set_binary(&self, name: &str, binary: BinaryPackage) {
        let mut cache = self.local.write().unwrap();
        cache.insert(name.to_string(), Some(binary));
    }

    // TODO: remove allow once BinFactory is refactored
    // currently fine because a BinFactory is only used by a single process tree
    #[allow(clippy::await_holding_lock)]
    pub async fn get_binary(
        &self,
        name: &str,
        fs: Option<&dyn FileSystem>,
    ) -> Option<BinaryPackage> {
        let name = name.to_string();

        // Fast path
        {
            let cache = self.local.read().unwrap();
            if let Some(data) = cache.get(&name) {
                return data.clone();
            }
        }

        // Slow path
        let mut cache = self.local.write().unwrap();

        // Check the cache
        if let Some(data) = cache.get(&name) {
            return data.clone();
        }

        // Check the filesystem for the file
        if name.starts_with('/') {
            if let Some(fs) = fs {
                match load_package_from_filesystem(fs, name.as_ref()).await {
                    Ok(pkg) => {
                        cache.insert(name, Some(pkg.clone()));
                        return Some(pkg);
                    }
                    Err(e) => {
                        tracing::warn!(
                            path = name,
                            error = &*e,
                            "Unable to load the package from disk"
                        );
                    }
                }
            }
        }

        // NAK
        cache.insert(name, None);
        None
    }
}

async fn load_package_from_filesystem(
    fs: &dyn FileSystem,
    path: &Path,
) -> Result<BinaryPackage, anyhow::Error> {
    let mut f = fs
        .new_open_options()
        .read(true)
        .open(path)
        .context("Unable to open the file")?;

    let mut data = Vec::with_capacity(f.size() as usize);
    f.read_to_end(&mut data).await.context("Read failed")?;
    let pkg = crate::wapm::parse_static_webc(data).context("Unable to parse the package")?;

    Ok(pkg)
}

pub fn hash_of_binary(data: impl AsRef<[u8]>) -> String {
    let mut hasher = Sha256::default();
    hasher.update(data.as_ref());
    let hash = hasher.finalize();
    hex::encode(&hash[..])
}
