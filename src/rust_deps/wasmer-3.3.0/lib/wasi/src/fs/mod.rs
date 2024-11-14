mod fd;
mod inode_guard;
mod notification;

use std::{
    borrow::{Borrow, Cow},
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering},
        Arc, Mutex, RwLock, Weak,
    },
};

use crate::state::{Stderr, Stdin, Stdout};
#[cfg(feature = "enable-serde")]
use serde_derive::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tracing::{debug, trace};
use virtual_fs::{FileSystem, FsError, OpenOptions, VirtualFile};
use wasmer_wasix_types::{
    types::{__WASI_STDERR_FILENO, __WASI_STDIN_FILENO, __WASI_STDOUT_FILENO},
    wasi::{
        Errno, Fd as WasiFd, Fdflags, Fdstat, Filesize, Filestat, Filetype, Preopentype, Prestat,
        PrestatEnum, Rights,
    },
};

pub use self::fd::{Fd, InodeVal, Kind};
pub(crate) use self::inode_guard::{
    InodeValFilePollGuard, InodeValFilePollGuardJoin, InodeValFileReadGuard,
    InodeValFileWriteGuard, WasiStateFileGuard,
};
pub use self::notification::NotificationInner;
use crate::syscalls::map_io_err;
use crate::{bin_factory::BinaryPackage, state::PreopenedDir, ALL_RIGHTS};

/// the fd value of the virtual root
pub const VIRTUAL_ROOT_FD: WasiFd = 3;

const STDIN_DEFAULT_RIGHTS: Rights = {
    // This might seem a bit overenineered, but it's the only way I
    // discovered for getting the values in a const environment
    Rights::from_bits_truncate(
        Rights::FD_DATASYNC.bits()
            | Rights::FD_READ.bits()
            | Rights::FD_SYNC.bits()
            | Rights::FD_ADVISE.bits()
            | Rights::FD_FILESTAT_GET.bits()
            | Rights::POLL_FD_READWRITE.bits(),
    )
};
const STDOUT_DEFAULT_RIGHTS: Rights = {
    // This might seem a bit overenineered, but it's the only way I
    // discovered for getting the values in a const environment
    Rights::from_bits_truncate(
        Rights::FD_DATASYNC.bits()
            | Rights::FD_SYNC.bits()
            | Rights::FD_WRITE.bits()
            | Rights::FD_ADVISE.bits()
            | Rights::FD_FILESTAT_GET.bits()
            | Rights::POLL_FD_READWRITE.bits(),
    )
};
const STDERR_DEFAULT_RIGHTS: Rights = STDOUT_DEFAULT_RIGHTS;

/// A completely aribtrary "big enough" number used as the upper limit for
/// the number of symlinks that can be traversed when resolving a path
pub const MAX_SYMLINKS: u32 = 128;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Inode(u64);

impl Inode {
    pub fn as_u64(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct InodeGuard {
    ino: Inode,
    inner: Arc<InodeVal>,
}
impl InodeGuard {
    pub fn ino(&self) -> Inode {
        self.ino
    }
    pub fn downgrade(&self) -> InodeWeakGuard {
        InodeWeakGuard {
            ino: self.ino,
            inner: Arc::downgrade(&self.inner),
        }
    }
    pub fn ref_cnt(&self) -> usize {
        Arc::strong_count(&self.inner)
    }
}
impl std::ops::Deref for InodeGuard {
    type Target = InodeVal;
    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

#[derive(Debug, Clone)]
pub struct InodeWeakGuard {
    ino: Inode,
    inner: Weak<InodeVal>,
}
impl InodeWeakGuard {
    pub fn ino(&self) -> Inode {
        self.ino
    }
    pub fn upgrade(&self) -> Option<InodeGuard> {
        Weak::upgrade(&self.inner).map(|inner| InodeGuard {
            ino: self.ino,
            inner,
        })
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "enable-serde", derive(Serialize, Deserialize))]
struct WasiInodesProtected {
    seed: u64,
    lookup: HashMap<Inode, Weak<InodeVal>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "enable-serde", derive(Serialize, Deserialize))]
pub struct WasiInodes {
    protected: Arc<RwLock<WasiInodesProtected>>,
}

impl WasiInodes {
    pub fn new() -> Self {
        Self {
            protected: Arc::new(RwLock::new(WasiInodesProtected {
                seed: 1,
                lookup: Default::default(),
            })),
        }
    }

    /// adds another value to the inodes
    pub fn add_inode_val(&self, val: InodeVal) -> InodeGuard {
        let val = Arc::new(val);

        let mut guard = self.protected.write().unwrap();
        let ino = Inode(guard.seed);
        guard.seed += 1;
        guard.lookup.insert(ino, Arc::downgrade(&val));

        // Set the inode value
        {
            let mut guard = val.stat.write().unwrap();
            guard.st_ino = ino.0;
        }

        // every 100 calls we clear out dead weaks
        if guard.seed % 100 == 1 {
            guard.lookup.retain(|_, v| Weak::strong_count(v) > 0);
        }

        InodeGuard { ino, inner: val }
    }

    /// Get the `VirtualFile` object at stdout
    pub(crate) fn stdout(
        fd_map: &RwLock<HashMap<u32, Fd>>,
    ) -> Result<InodeValFileReadGuard, FsError> {
        Self::std_dev_get(fd_map, __WASI_STDOUT_FILENO)
    }
    /// Get the `VirtualFile` object at stdout mutably
    pub(crate) fn stdout_mut(
        fd_map: &RwLock<HashMap<u32, Fd>>,
    ) -> Result<InodeValFileWriteGuard, FsError> {
        Self::std_dev_get_mut(fd_map, __WASI_STDOUT_FILENO)
    }

    /// Get the `VirtualFile` object at stderr
    pub(crate) fn stderr(
        fd_map: &RwLock<HashMap<u32, Fd>>,
    ) -> Result<InodeValFileReadGuard, FsError> {
        Self::std_dev_get(fd_map, __WASI_STDERR_FILENO)
    }
    /// Get the `VirtualFile` object at stderr mutably
    pub(crate) fn stderr_mut(
        fd_map: &RwLock<HashMap<u32, Fd>>,
    ) -> Result<InodeValFileWriteGuard, FsError> {
        Self::std_dev_get_mut(fd_map, __WASI_STDERR_FILENO)
    }

    /// Get the `VirtualFile` object at stdin
    /// TODO: Review why this is dead
    #[allow(dead_code)]
    pub(crate) fn stdin(
        fd_map: &RwLock<HashMap<u32, Fd>>,
    ) -> Result<InodeValFileReadGuard, FsError> {
        Self::std_dev_get(fd_map, __WASI_STDIN_FILENO)
    }
    /// Get the `VirtualFile` object at stdin mutably
    pub(crate) fn stdin_mut(
        fd_map: &RwLock<HashMap<u32, Fd>>,
    ) -> Result<InodeValFileWriteGuard, FsError> {
        Self::std_dev_get_mut(fd_map, __WASI_STDIN_FILENO)
    }

    /// Internal helper function to get a standard device handle.
    /// Expects one of `__WASI_STDIN_FILENO`, `__WASI_STDOUT_FILENO`, `__WASI_STDERR_FILENO`.
    fn std_dev_get(
        fd_map: &RwLock<HashMap<u32, Fd>>,
        fd: WasiFd,
    ) -> Result<InodeValFileReadGuard, FsError> {
        if let Some(fd) = fd_map.read().unwrap().get(&fd) {
            let guard = fd.inode.read();
            if let Kind::File {
                handle: Some(handle),
                ..
            } = guard.deref()
            {
                Ok(InodeValFileReadGuard::new(handle))
            } else {
                // Our public API should ensure that this is not possible
                Err(FsError::NotAFile)
            }
        } else {
            // this should only trigger if we made a mistake in this crate
            Err(FsError::NoDevice)
        }
    }
    /// Internal helper function to mutably get a standard device handle.
    /// Expects one of `__WASI_STDIN_FILENO`, `__WASI_STDOUT_FILENO`, `__WASI_STDERR_FILENO`.
    fn std_dev_get_mut(
        fd_map: &RwLock<HashMap<u32, Fd>>,
        fd: WasiFd,
    ) -> Result<InodeValFileWriteGuard, FsError> {
        if let Some(fd) = fd_map.read().unwrap().get(&fd) {
            let guard = fd.inode.read();
            if let Kind::File {
                handle: Some(handle),
                ..
            } = guard.deref()
            {
                Ok(InodeValFileWriteGuard::new(handle))
            } else {
                // Our public API should ensure that this is not possible
                Err(FsError::NotAFile)
            }
        } else {
            // this should only trigger if we made a mistake in this crate
            Err(FsError::NoDevice)
        }
    }
}

impl Default for WasiInodes {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum WasiFsRoot {
    Sandbox(Arc<virtual_fs::tmp_fs::TmpFileSystem>),
    Backing(Arc<Box<dyn FileSystem>>),
}

impl FileSystem for WasiFsRoot {
    fn read_dir(&self, path: &Path) -> virtual_fs::Result<virtual_fs::ReadDir> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.read_dir(path),
            WasiFsRoot::Backing(fs) => fs.read_dir(path),
        }
    }
    fn create_dir(&self, path: &Path) -> virtual_fs::Result<()> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.create_dir(path),
            WasiFsRoot::Backing(fs) => fs.create_dir(path),
        }
    }
    fn remove_dir(&self, path: &Path) -> virtual_fs::Result<()> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.remove_dir(path),
            WasiFsRoot::Backing(fs) => fs.remove_dir(path),
        }
    }
    fn rename(&self, from: &Path, to: &Path) -> virtual_fs::Result<()> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.rename(from, to),
            WasiFsRoot::Backing(fs) => fs.rename(from, to),
        }
    }
    fn metadata(&self, path: &Path) -> virtual_fs::Result<virtual_fs::Metadata> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.metadata(path),
            WasiFsRoot::Backing(fs) => fs.metadata(path),
        }
    }
    fn symlink_metadata(&self, path: &Path) -> virtual_fs::Result<virtual_fs::Metadata> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.symlink_metadata(path),
            WasiFsRoot::Backing(fs) => fs.symlink_metadata(path),
        }
    }
    fn remove_file(&self, path: &Path) -> virtual_fs::Result<()> {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.remove_file(path),
            WasiFsRoot::Backing(fs) => fs.remove_file(path),
        }
    }
    fn new_open_options(&self) -> OpenOptions {
        match self {
            WasiFsRoot::Sandbox(fs) => fs.new_open_options(),
            WasiFsRoot::Backing(fs) => fs.new_open_options(),
        }
    }
}

/// Warning, modifying these fields directly may cause invariants to break and
/// should be considered unsafe.  These fields may be made private in a future release
#[cfg_attr(feature = "enable-serde", derive(Serialize, Deserialize))]
pub struct WasiFs {
    //pub repo: Repo,
    pub preopen_fds: RwLock<Vec<u32>>,
    pub name_map: HashMap<String, Inode>,
    pub fd_map: Arc<RwLock<HashMap<WasiFd, Fd>>>,
    pub next_fd: AtomicU32,
    pub current_dir: Mutex<String>,
    #[cfg_attr(feature = "enable-serde", serde(skip, default))]
    pub root_fs: WasiFsRoot,
    pub root_inode: InodeGuard,
    pub has_unioned: Arc<Mutex<HashSet<String>>>,

    // TODO: remove
    // using an atomic is a hack to enable customization after construction,
    // but it shouldn't be necessary
    // It should not be necessary at all.
    is_wasix: AtomicBool,
}

impl WasiFs {
    pub fn is_wasix(&self) -> bool {
        // NOTE: this will only be set once very early in the instance lifetime,
        // so Relaxed should be okay.
        self.is_wasix.load(Ordering::Relaxed)
    }

    pub fn set_is_wasix(&self, is_wasix: bool) {
        self.is_wasix.store(is_wasix, Ordering::SeqCst);
    }

    /// Forking the WasiState is used when either fork or vfork is called
    pub fn fork(&self) -> Self {
        let fd_map = self.fd_map.read().unwrap().clone();
        Self {
            preopen_fds: RwLock::new(self.preopen_fds.read().unwrap().clone()),
            name_map: self.name_map.clone(),
            fd_map: Arc::new(RwLock::new(fd_map)),
            next_fd: AtomicU32::new(self.next_fd.load(Ordering::SeqCst)),
            current_dir: Mutex::new(self.current_dir.lock().unwrap().clone()),
            is_wasix: AtomicBool::new(self.is_wasix.load(Ordering::Acquire)),
            root_fs: self.root_fs.clone(),
            root_inode: self.root_inode.clone(),
            has_unioned: Arc::new(Mutex::new(HashSet::new())),
        }
    }

    /// Closes all the file handles.
    #[allow(clippy::await_holding_lock)]
    pub async fn close_all(&self) {
        // TODO: this should close all uniquely owned files instead of just flushing.

        // Make sure the STDOUT and STDERR are explicitely flushed
        self.flush(__WASI_STDOUT_FILENO).await.ok();
        self.flush(__WASI_STDERR_FILENO).await.ok();

        let to_close = {
            if let Ok(map) = self.fd_map.read() {
                map.keys().copied().collect::<Vec<_>>()
            } else {
                Vec::new()
            }
        };

        for fd in to_close {
            self.flush(fd).await.ok();
            self.close_fd(fd).ok();
        }

        if let Ok(mut map) = self.fd_map.write() {
            map.clear();
        }
    }

    /// Will conditionally union the binary file system with this one
    /// if it has not already been unioned
    pub fn conditional_union(&self, binary: &BinaryPackage) -> bool {
        let sandbox_fs = match &self.root_fs {
            WasiFsRoot::Sandbox(fs) => fs,
            WasiFsRoot::Backing(_) => {
                tracing::error!("can not perform a union on a backing file system");
                return false;
            }
        };
        let package_name = binary.package_name.to_string();
        let mut guard = self.has_unioned.lock().unwrap();
        if !guard.contains(&package_name) {
            guard.insert(package_name);

            if let Some(fs) = binary.webc_fs.clone() {
                sandbox_fs.union(&fs);
            }
        }
        true
    }

    /// Created for the builder API. like `new` but with more information
    pub(crate) fn new_with_preopen(
        inodes: &WasiInodes,
        preopens: &[PreopenedDir],
        vfs_preopens: &[String],
        fs_backing: WasiFsRoot,
    ) -> Result<Self, String> {
        let (wasi_fs, root_inode) = Self::new_init(fs_backing, inodes)?;

        for preopen_name in vfs_preopens {
            let kind = Kind::Dir {
                parent: root_inode.downgrade(),
                path: PathBuf::from(preopen_name),
                entries: Default::default(),
            };
            let rights = Rights::FD_ADVISE
                | Rights::FD_TELL
                | Rights::FD_SEEK
                | Rights::FD_READ
                | Rights::PATH_OPEN
                | Rights::FD_READDIR
                | Rights::PATH_READLINK
                | Rights::PATH_FILESTAT_GET
                | Rights::FD_FILESTAT_GET
                | Rights::PATH_LINK_SOURCE
                | Rights::PATH_RENAME_SOURCE
                | Rights::POLL_FD_READWRITE
                | Rights::SOCK_SHUTDOWN;
            let inode = wasi_fs
                .create_inode(inodes, kind, true, preopen_name.clone())
                .map_err(|e| {
                    format!(
                        "Failed to create inode for preopened dir (name `{}`): WASI error code: {}",
                        preopen_name, e
                    )
                })?;
            let fd_flags = Fd::READ;
            let fd = wasi_fs
                .create_fd(rights, rights, Fdflags::empty(), fd_flags, inode.clone())
                .map_err(|e| format!("Could not open fd for file {:?}: {}", preopen_name, e))?;
            {
                let mut guard = root_inode.write();
                if let Kind::Root { entries } = guard.deref_mut() {
                    let existing_entry = entries.insert(preopen_name.clone(), inode);
                    if existing_entry.is_some() {
                        return Err(format!(
                            "Found duplicate entry for alias `{}`",
                            preopen_name
                        ));
                    }
                    assert!(existing_entry.is_none())
                }
            }
            wasi_fs.preopen_fds.write().unwrap().push(fd);
        }

        for PreopenedDir {
            path,
            alias,
            read,
            write,
            create,
        } in preopens
        {
            debug!(
                "Attempting to preopen {} with alias {:?}",
                &path.to_string_lossy(),
                &alias
            );
            let cur_dir_metadata = wasi_fs
                .root_fs
                .metadata(path)
                .map_err(|e| format!("Could not get metadata for file {:?}: {}", path, e))?;

            let kind = if cur_dir_metadata.is_dir() {
                Kind::Dir {
                    parent: root_inode.downgrade(),
                    path: path.clone(),
                    entries: Default::default(),
                }
            } else {
                return Err(format!(
                    "WASI only supports pre-opened directories right now; found \"{}\"",
                    &path.to_string_lossy()
                ));
            };

            let rights = {
                // TODO: review tell' and fd_readwrite
                let mut rights = Rights::FD_ADVISE | Rights::FD_TELL | Rights::FD_SEEK;
                if *read {
                    rights |= Rights::FD_READ
                        | Rights::PATH_OPEN
                        | Rights::FD_READDIR
                        | Rights::PATH_READLINK
                        | Rights::PATH_FILESTAT_GET
                        | Rights::FD_FILESTAT_GET
                        | Rights::PATH_LINK_SOURCE
                        | Rights::PATH_RENAME_SOURCE
                        | Rights::POLL_FD_READWRITE
                        | Rights::SOCK_SHUTDOWN;
                }
                if *write {
                    rights |= Rights::FD_DATASYNC
                        | Rights::FD_FDSTAT_SET_FLAGS
                        | Rights::FD_WRITE
                        | Rights::FD_SYNC
                        | Rights::FD_ALLOCATE
                        | Rights::PATH_OPEN
                        | Rights::PATH_RENAME_TARGET
                        | Rights::PATH_FILESTAT_SET_SIZE
                        | Rights::PATH_FILESTAT_SET_TIMES
                        | Rights::FD_FILESTAT_SET_SIZE
                        | Rights::FD_FILESTAT_SET_TIMES
                        | Rights::PATH_REMOVE_DIRECTORY
                        | Rights::PATH_UNLINK_FILE
                        | Rights::POLL_FD_READWRITE
                        | Rights::SOCK_SHUTDOWN;
                }
                if *create {
                    rights |= Rights::PATH_CREATE_DIRECTORY
                        | Rights::PATH_CREATE_FILE
                        | Rights::PATH_LINK_TARGET
                        | Rights::PATH_OPEN
                        | Rights::PATH_RENAME_TARGET
                        | Rights::PATH_SYMLINK;
                }

                rights
            };
            let inode = if let Some(alias) = &alias {
                wasi_fs.create_inode(inodes, kind, true, alias.clone())
            } else {
                wasi_fs.create_inode(inodes, kind, true, path.to_string_lossy().into_owned())
            }
            .map_err(|e| {
                format!(
                    "Failed to create inode for preopened dir: WASI error code: {}",
                    e
                )
            })?;
            let fd_flags = {
                let mut fd_flags = 0;
                if *read {
                    fd_flags |= Fd::READ;
                }
                if *write {
                    // TODO: introduce API for finer grained control
                    fd_flags |= Fd::WRITE | Fd::APPEND | Fd::TRUNCATE;
                }
                if *create {
                    fd_flags |= Fd::CREATE;
                }
                fd_flags
            };
            let fd = wasi_fs
                .create_fd(rights, rights, Fdflags::empty(), fd_flags, inode.clone())
                .map_err(|e| format!("Could not open fd for file {:?}: {}", path, e))?;
            {
                let mut guard = root_inode.write();
                if let Kind::Root { entries } = guard.deref_mut() {
                    let key = if let Some(alias) = &alias {
                        alias.clone()
                    } else {
                        path.to_string_lossy().into_owned()
                    };
                    let existing_entry = entries.insert(key.clone(), inode);
                    if existing_entry.is_some() {
                        return Err(format!("Found duplicate entry for alias `{}`", key));
                    }
                    assert!(existing_entry.is_none())
                }
            }
            wasi_fs.preopen_fds.write().unwrap().push(fd);
        }

        Ok(wasi_fs)
    }

    /// Converts a relative path into an absolute path
    pub(crate) fn relative_path_to_absolute(&self, mut path: String) -> String {
        if path.starts_with("./") {
            let current_dir = self.current_dir.lock().unwrap();
            path = format!("{}{}", current_dir.as_str(), &path[1..]);
            if path.contains("//") {
                path = path.replace("//", "/");
            }
        }
        path
    }

    /// Private helper function to init the filesystem, called in `new` and
    /// `new_with_preopen`
    fn new_init(fs_backing: WasiFsRoot, inodes: &WasiInodes) -> Result<(Self, InodeGuard), String> {
        debug!("Initializing WASI filesystem");

        let stat = Filestat {
            st_filetype: Filetype::Directory,
            ..Filestat::default()
        };
        let root_kind = Kind::Root {
            entries: HashMap::new(),
        };
        let root_inode = inodes.add_inode_val(InodeVal {
            stat: RwLock::new(stat),
            is_preopened: true,
            name: "/".into(),
            kind: RwLock::new(root_kind),
        });

        let wasi_fs = Self {
            preopen_fds: RwLock::new(vec![]),
            name_map: HashMap::new(),
            fd_map: Arc::new(RwLock::new(HashMap::new())),
            next_fd: AtomicU32::new(3),
            current_dir: Mutex::new("/".to_string()),
            is_wasix: AtomicBool::new(false),
            root_fs: fs_backing,
            root_inode: root_inode.clone(),
            has_unioned: Arc::new(Mutex::new(HashSet::new())),
        };
        wasi_fs.create_stdin(inodes);
        wasi_fs.create_stdout(inodes);
        wasi_fs.create_stderr(inodes);

        // create virtual root
        let all_rights = ALL_RIGHTS;
        // TODO: make this a list of positive rigths instead of negative ones
        // root gets all right for now
        let root_rights = all_rights
            /*
            & (!Rights::FD_WRITE)
            & (!Rights::FD_ALLOCATE)
            & (!Rights::PATH_CREATE_DIRECTORY)
            & (!Rights::PATH_CREATE_FILE)
            & (!Rights::PATH_LINK_SOURCE)
            & (!Rights::PATH_RENAME_SOURCE)
            & (!Rights::PATH_RENAME_TARGET)
            & (!Rights::PATH_FILESTAT_SET_SIZE)
            & (!Rights::PATH_FILESTAT_SET_TIMES)
            & (!Rights::FD_FILESTAT_SET_SIZE)
            & (!Rights::FD_FILESTAT_SET_TIMES)
            & (!Rights::PATH_SYMLINK)
            & (!Rights::PATH_UNLINK_FILE)
            & (!Rights::PATH_REMOVE_DIRECTORY)
            */;
        let fd = wasi_fs
            .create_fd(
                root_rights,
                root_rights,
                Fdflags::empty(),
                Fd::READ,
                root_inode.clone(),
            )
            .map_err(|e| format!("Could not create root fd: {}", e))?;
        wasi_fs.preopen_fds.write().unwrap().push(fd);

        Ok((wasi_fs, root_inode))
    }

    /// This function is like create dir all, but it also opens it.
    /// Function is unsafe because it may break invariants and hasn't been tested.
    /// This is an experimental function and may be removed
    ///
    /// # Safety
    /// - Virtual directories created with this function must not conflict with
    ///   the standard operation of the WASI filesystem.  This is vague and
    ///   unlikely in pratice.  [Join the discussion](https://github.com/wasmerio/wasmer/issues/1219)
    ///   for what the newer, safer WASI FS APIs should look like.
    #[allow(dead_code)]
    pub unsafe fn open_dir_all(
        &mut self,
        inodes: &WasiInodes,
        base: WasiFd,
        name: String,
        rights: Rights,
        rights_inheriting: Rights,
        flags: Fdflags,
    ) -> Result<WasiFd, FsError> {
        // TODO: check permissions here? probably not, but this should be
        // an explicit choice, so justify it in a comment when we remove this one
        let mut cur_inode = self.get_fd_inode(base).map_err(fs_error_from_wasi_err)?;

        let path: &Path = Path::new(&name);
        //let n_components = path.components().count();
        for c in path.components() {
            let segment_name = c.as_os_str().to_string_lossy().to_string();
            let guard = cur_inode.read();
            match guard.deref() {
                Kind::Dir { ref entries, .. } | Kind::Root { ref entries } => {
                    if let Some(_entry) = entries.get(&segment_name) {
                        // TODO: this should be fixed
                        return Err(FsError::AlreadyExists);
                    }

                    let kind = Kind::Dir {
                        parent: cur_inode.downgrade(),
                        path: PathBuf::from(""),
                        entries: HashMap::new(),
                    };

                    drop(guard);
                    let inode = self.create_inode_with_default_stat(
                        inodes,
                        kind,
                        false,
                        segment_name.clone().into(),
                    );

                    // reborrow to insert
                    {
                        let mut guard = cur_inode.write();
                        match guard.deref_mut() {
                            Kind::Dir {
                                ref mut entries, ..
                            }
                            | Kind::Root { ref mut entries } => {
                                entries.insert(segment_name, inode.clone());
                            }
                            _ => unreachable!("Dir or Root became not Dir or Root"),
                        }
                    }
                    cur_inode = inode;
                }
                _ => return Err(FsError::BaseNotDirectory),
            }
        }

        // TODO: review open flags (read, write); they were added without consideration
        self.create_fd(
            rights,
            rights_inheriting,
            flags,
            Fd::READ | Fd::WRITE,
            cur_inode,
        )
        .map_err(fs_error_from_wasi_err)
    }

    /// Opens a user-supplied file in the directory specified with the
    /// name and flags given
    // dead code because this is an API for external use
    #[allow(dead_code, clippy::too_many_arguments)]
    pub fn open_file_at(
        &mut self,
        inodes: &WasiInodes,
        base: WasiFd,
        file: Box<dyn VirtualFile + Send + Sync + 'static>,
        open_flags: u16,
        name: String,
        rights: Rights,
        rights_inheriting: Rights,
        flags: Fdflags,
    ) -> Result<WasiFd, FsError> {
        // TODO: check permissions here? probably not, but this should be
        // an explicit choice, so justify it in a comment when we remove this one
        let base_inode = self.get_fd_inode(base).map_err(fs_error_from_wasi_err)?;

        let guard = base_inode.read();
        match guard.deref() {
            Kind::Dir { ref entries, .. } | Kind::Root { ref entries } => {
                if let Some(_entry) = entries.get(&name) {
                    // TODO: eventually change the logic here to allow overwrites
                    return Err(FsError::AlreadyExists);
                }

                let kind = Kind::File {
                    handle: Some(Arc::new(RwLock::new(file))),
                    path: PathBuf::from(""),
                    fd: Some(self.next_fd.fetch_add(1, Ordering::SeqCst)),
                };

                drop(guard);
                let inode = self
                    .create_inode(inodes, kind, false, name.clone())
                    .map_err(|_| FsError::IOError)?;

                {
                    let mut guard = base_inode.write();
                    match guard.deref_mut() {
                        Kind::Dir {
                            ref mut entries, ..
                        }
                        | Kind::Root { ref mut entries } => {
                            entries.insert(name, inode.clone());
                        }
                        _ => unreachable!("Dir or Root became not Dir or Root"),
                    }
                }

                self.create_fd(rights, rights_inheriting, flags, open_flags, inode)
                    .map_err(fs_error_from_wasi_err)
            }
            _ => Err(FsError::BaseNotDirectory),
        }
    }

    /// Change the backing of a given file descriptor
    /// Returns the old backing
    /// TODO: add examples
    #[allow(dead_code)]
    pub fn swap_file(
        &self,
        fd: WasiFd,
        mut file: Box<dyn VirtualFile + Send + Sync + 'static>,
    ) -> Result<Option<Box<dyn VirtualFile + Send + Sync + 'static>>, FsError> {
        match fd {
            __WASI_STDIN_FILENO => {
                let mut target = WasiInodes::stdin_mut(&self.fd_map)?;
                Ok(Some(target.swap(file)))
            }
            __WASI_STDOUT_FILENO => {
                let mut target = WasiInodes::stdout_mut(&self.fd_map)?;
                Ok(Some(target.swap(file)))
            }
            __WASI_STDERR_FILENO => {
                let mut target = WasiInodes::stderr_mut(&self.fd_map)?;
                Ok(Some(target.swap(file)))
            }
            _ => {
                let base_inode = self.get_fd_inode(fd).map_err(fs_error_from_wasi_err)?;
                {
                    // happy path
                    let guard = base_inode.read();
                    match guard.deref() {
                        Kind::File { ref handle, .. } => {
                            if let Some(handle) = handle {
                                let mut handle = handle.write().unwrap();
                                std::mem::swap(handle.deref_mut(), &mut file);
                                return Ok(Some(file));
                            }
                        }
                        _ => return Err(FsError::NotAFile),
                    }
                }
                // slow path
                let mut guard = base_inode.write();
                match guard.deref_mut() {
                    Kind::File { ref mut handle, .. } => {
                        if let Some(handle) = handle {
                            let mut handle = handle.write().unwrap();
                            std::mem::swap(handle.deref_mut(), &mut file);
                            Ok(Some(file))
                        } else {
                            handle.replace(Arc::new(RwLock::new(file)));
                            Ok(None)
                        }
                    }
                    _ => Err(FsError::NotAFile),
                }
            }
        }
    }

    /// refresh size from filesystem
    pub fn filestat_resync_size(&self, fd: WasiFd) -> Result<Filesize, Errno> {
        let inode = self.get_fd_inode(fd)?;
        let mut guard = inode.write();
        match guard.deref_mut() {
            Kind::File { handle, .. } => {
                if let Some(h) = handle {
                    let h = h.read().unwrap();
                    let new_size = h.size();
                    drop(h);
                    drop(guard);

                    inode.stat.write().unwrap().st_size = new_size;
                    Ok(new_size as Filesize)
                } else {
                    Err(Errno::Badf)
                }
            }
            Kind::Dir { .. } | Kind::Root { .. } => Err(Errno::Isdir),
            _ => Err(Errno::Inval),
        }
    }

    /// Changes the current directory
    pub fn set_current_dir(&self, path: &str) {
        let mut guard = self.current_dir.lock().unwrap();
        *guard = path.to_string();
    }

    /// Gets the current directory
    pub fn get_current_dir(
        &self,
        inodes: &WasiInodes,
        base: WasiFd,
    ) -> Result<(InodeGuard, String), Errno> {
        self.get_current_dir_inner(inodes, base, 0)
    }

    pub(crate) fn get_current_dir_inner(
        &self,
        inodes: &WasiInodes,
        base: WasiFd,
        symlink_count: u32,
    ) -> Result<(InodeGuard, String), Errno> {
        let current_dir = {
            let guard = self.current_dir.lock().unwrap();
            guard.clone()
        };
        let cur_inode = self.get_fd_inode(base)?;
        let inode = self.get_inode_at_path_inner(
            inodes,
            cur_inode,
            current_dir.as_str(),
            symlink_count,
            true,
        )?;
        Ok((inode, current_dir))
    }

    /// Internal part of the core path resolution function which implements path
    /// traversal logic such as resolving relative path segments (such as
    /// `.` and `..`) and resolving symlinks (while preventing infinite
    /// loops/stack overflows).
    ///
    /// TODO: expand upon exactly what the state of the returned value is,
    /// explaining lazy-loading from the real file system and synchronizing
    /// between them.
    ///
    /// This is where a lot of the magic happens, be very careful when editing
    /// this code.
    ///
    /// TODO: write more tests for this code
    fn get_inode_at_path_inner(
        &self,
        inodes: &WasiInodes,
        mut cur_inode: InodeGuard,
        path: &str,
        mut symlink_count: u32,
        follow_symlinks: bool,
    ) -> Result<InodeGuard, Errno> {
        if symlink_count > MAX_SYMLINKS {
            return Err(Errno::Mlink);
        }

        let path: &Path = Path::new(path);
        let n_components = path.components().count();

        // TODO: rights checks
        'path_iter: for (i, component) in path.components().enumerate() {
            // used to terminate symlink resolution properly
            let last_component = i + 1 == n_components;
            // for each component traverse file structure
            // loading inodes as necessary
            'symlink_resolution: while symlink_count < MAX_SYMLINKS {
                let processing_cur_inode = cur_inode.clone();
                let mut guard = processing_cur_inode.write();
                match guard.deref_mut() {
                    Kind::Buffer { .. } => unimplemented!("state::get_inode_at_path for buffers"),
                    Kind::Dir {
                        ref mut entries,
                        ref path,
                        ref parent,
                        ..
                    } => {
                        match component.as_os_str().to_string_lossy().borrow() {
                            ".." => {
                                if let Some(p) = parent.upgrade() {
                                    cur_inode = p;
                                    continue 'path_iter;
                                } else {
                                    return Err(Errno::Access);
                                }
                            }
                            "." => continue 'path_iter,
                            _ => (),
                        }
                        // used for full resolution of symlinks
                        let mut loop_for_symlink = false;
                        if let Some(entry) =
                            entries.get(component.as_os_str().to_string_lossy().as_ref())
                        {
                            cur_inode = entry.clone();
                        } else {
                            let file = {
                                let mut cd = path.clone();
                                cd.push(component);
                                cd
                            };
                            let metadata = self
                                .root_fs
                                .symlink_metadata(&file)
                                .ok()
                                .ok_or(Errno::Noent)?;
                            let file_type = metadata.file_type();
                            // we want to insert newly opened dirs and files, but not transient symlinks
                            // TODO: explain why (think about this deeply when well rested)
                            let should_insert;

                            let kind = if file_type.is_dir() {
                                should_insert = true;
                                // load DIR
                                Kind::Dir {
                                    parent: cur_inode.downgrade(),
                                    path: file.clone(),
                                    entries: Default::default(),
                                }
                            } else if file_type.is_file() {
                                should_insert = true;
                                // load file
                                Kind::File {
                                    handle: None,
                                    path: file.clone(),
                                    fd: None,
                                }
                            } else if file_type.is_symlink() {
                                should_insert = false;
                                let link_value = file.read_link().map_err(map_io_err)?;
                                debug!("attempting to decompose path {:?}", link_value);

                                let (pre_open_dir_fd, relative_path) = if link_value.is_relative() {
                                    self.path_into_pre_open_and_relative_path(&file)?
                                } else {
                                    unimplemented!("Absolute symlinks are not yet supported");
                                };
                                loop_for_symlink = true;
                                symlink_count += 1;
                                Kind::Symlink {
                                    base_po_dir: pre_open_dir_fd,
                                    path_to_symlink: relative_path.to_owned(),
                                    relative_path: link_value,
                                }
                            } else {
                                #[cfg(unix)]
                                {
                                    //use std::os::unix::fs::FileTypeExt;
                                    let file_type: Filetype = if file_type.is_char_device() {
                                        Filetype::CharacterDevice
                                    } else if file_type.is_block_device() {
                                        Filetype::BlockDevice
                                    } else if file_type.is_fifo() {
                                        // FIFO doesn't seem to fit any other type, so unknown
                                        Filetype::Unknown
                                    } else if file_type.is_socket() {
                                        // TODO: how do we know if it's a `SocketStream` or
                                        // a `SocketDgram`?
                                        Filetype::SocketStream
                                    } else {
                                        unimplemented!("state::get_inode_at_path unknown file type: not file, directory, symlink, char device, block device, fifo, or socket");
                                    };

                                    let kind = Kind::File {
                                        handle: None,
                                        path: file.clone(),
                                        fd: None,
                                    };
                                    drop(guard);
                                    let new_inode = self.create_inode_with_stat(
                                        inodes,
                                        kind,
                                        false,
                                        file.to_string_lossy().to_string().into(),
                                        Filestat {
                                            st_filetype: file_type,
                                            ..Filestat::default()
                                        },
                                    );

                                    let mut guard = cur_inode.write();
                                    if let Kind::Dir {
                                        ref mut entries, ..
                                    } = guard.deref_mut()
                                    {
                                        entries.insert(
                                            component.as_os_str().to_string_lossy().to_string(),
                                            new_inode.clone(),
                                        );
                                    } else {
                                        unreachable!(
                                            "Attempted to insert special device into non-directory"
                                        );
                                    }
                                    // perhaps just continue with symlink resolution and return at the end
                                    return Ok(new_inode);
                                }
                                #[cfg(not(unix))]
                                unimplemented!("state::get_inode_at_path unknown file type: not file, directory, or symlink");
                            };
                            drop(guard);

                            let new_inode = self.create_inode(
                                inodes,
                                kind,
                                false,
                                file.to_string_lossy().to_string(),
                            )?;
                            if should_insert {
                                let mut guard = processing_cur_inode.write();
                                if let Kind::Dir {
                                    ref mut entries, ..
                                } = guard.deref_mut()
                                {
                                    entries.insert(
                                        component.as_os_str().to_string_lossy().to_string(),
                                        new_inode.clone(),
                                    );
                                }
                            }
                            cur_inode = new_inode;

                            if loop_for_symlink && follow_symlinks {
                                debug!("Following symlink to {:?}", cur_inode);
                                continue 'symlink_resolution;
                            }
                        }
                    }
                    Kind::Root { entries } => {
                        match component.as_os_str().to_string_lossy().borrow() {
                            // the root's parent is the root
                            ".." => continue 'path_iter,
                            // the root's current directory is the root
                            "." => continue 'path_iter,
                            _ => (),
                        }

                        if let Some(entry) =
                            entries.get(component.as_os_str().to_string_lossy().as_ref())
                        {
                            cur_inode = entry.clone();
                        } else {
                            // Root is not capable of having something other then preopenned folders
                            return Err(Errno::Notcapable);
                        }
                    }
                    Kind::File { .. }
                    | Kind::Socket { .. }
                    | Kind::Pipe { .. }
                    | Kind::EventNotifications { .. } => {
                        return Err(Errno::Notdir);
                    }
                    Kind::Symlink {
                        base_po_dir,
                        path_to_symlink,
                        relative_path,
                    } => {
                        let new_base_dir = *base_po_dir;
                        let new_base_inode = self.get_fd_inode(new_base_dir)?;

                        // allocate to reborrow mutabily to recur
                        let new_path = {
                            /*if let Kind::Root { .. } = self.inodes[base_po_dir].kind {
                                assert!(false, "symlinks should never be relative to the root");
                            }*/
                            let mut base = path_to_symlink.clone();
                            // remove the symlink file itself from the path, leaving just the path from the base
                            // to the dir containing the symlink
                            base.pop();
                            base.push(relative_path);
                            base.to_string_lossy().to_string()
                        };
                        debug!("Following symlink recursively");
                        drop(guard);
                        let symlink_inode = self.get_inode_at_path_inner(
                            inodes,
                            new_base_inode,
                            &new_path,
                            symlink_count + 1,
                            follow_symlinks,
                        )?;
                        cur_inode = symlink_inode;
                        // if we're at the very end and we found a file, then we're done
                        // TODO: figure out if this should also happen for directories?
                        let guard = cur_inode.read();
                        if let Kind::File { .. } = guard.deref() {
                            // check if on last step
                            if last_component {
                                break 'symlink_resolution;
                            }
                        }
                        continue 'symlink_resolution;
                    }
                }
                break 'symlink_resolution;
            }
        }

        Ok(cur_inode)
    }

    /// Finds the preopened directory that is the "best match" for the given path and
    /// returns a path relative to this preopened directory.
    ///
    /// The "best match" is the preopened directory that has the longest prefix of the
    /// given path. For example, given preopened directories [`a`, `a/b`, `a/c`] and
    /// the path `a/b/c/file`, we will return the fd corresponding to the preopened
    /// directory, `a/b` and the relative path `c/file`.
    ///
    /// In the case of a tie, the later preopened fd is preferred.
    fn path_into_pre_open_and_relative_path<'path>(
        &self,
        path: &'path Path,
    ) -> Result<(WasiFd, &'path Path), Errno> {
        enum BaseFdAndRelPath<'a> {
            None,
            BestMatch {
                fd: WasiFd,
                rel_path: &'a Path,
                max_seen: usize,
            },
        }

        impl<'a> BaseFdAndRelPath<'a> {
            const fn max_seen(&self) -> usize {
                match self {
                    Self::None => 0,
                    Self::BestMatch { max_seen, .. } => *max_seen,
                }
            }
        }
        let mut res = BaseFdAndRelPath::None;
        // for each preopened directory
        let preopen_fds = self.preopen_fds.read().unwrap();
        for po_fd in preopen_fds.deref() {
            let po_inode = self.fd_map.read().unwrap()[po_fd].inode.clone();
            let guard = po_inode.read();
            let po_path = match guard.deref() {
                Kind::Dir { path, .. } => &**path,
                Kind::Root { .. } => Path::new("/"),
                _ => unreachable!("Preopened FD that's not a directory or the root"),
            };
            // stem path based on it
            if let Ok(stripped_path) = path.strip_prefix(po_path) {
                // find the max
                let new_prefix_len = po_path.as_os_str().len();
                // we use >= to favor later preopens because we iterate in order
                // whereas WASI libc iterates in reverse to get this behavior.
                if new_prefix_len >= res.max_seen() {
                    res = BaseFdAndRelPath::BestMatch {
                        fd: *po_fd,
                        rel_path: stripped_path,
                        max_seen: new_prefix_len,
                    };
                }
            }
        }
        match res {
            // this error may not make sense depending on where it's called
            BaseFdAndRelPath::None => Err(Errno::Inval),
            BaseFdAndRelPath::BestMatch { fd, rel_path, .. } => Ok((fd, rel_path)),
        }
    }

    /// finds the number of directories between the fd and the inode if they're connected
    /// expects inode to point to a directory
    pub(crate) fn path_depth_from_fd(&self, fd: WasiFd, inode: InodeGuard) -> Result<usize, Errno> {
        let mut counter = 0;
        let base_inode = self.get_fd_inode(fd)?;
        let mut cur_inode = inode;

        while cur_inode.ino() != base_inode.ino() {
            counter += 1;

            let processing_cur_inode = cur_inode.clone();
            let guard = processing_cur_inode.read();

            match guard.deref() {
                Kind::Dir { parent, .. } => {
                    if let Some(p) = parent.upgrade() {
                        cur_inode = p;
                    }
                }
                _ => return Err(Errno::Inval),
            }
        }

        Ok(counter)
    }

    /// gets a host file from a base directory and a path
    /// this function ensures the fs remains sandboxed
    // NOTE: follow symlinks is super weird right now
    // even if it's false, it still follows symlinks, just not the last
    // symlink so
    // This will be resolved when we have tests asserting the correct behavior
    pub(crate) fn get_inode_at_path(
        &self,
        inodes: &WasiInodes,
        base: WasiFd,
        path: &str,
        follow_symlinks: bool,
    ) -> Result<InodeGuard, Errno> {
        let start_inode = if !path.starts_with('/') && self.is_wasix.load(Ordering::Acquire) {
            let (cur_inode, _) = self.get_current_dir(inodes, base)?;
            cur_inode
        } else {
            self.get_fd_inode(base)?
        };
        self.get_inode_at_path_inner(inodes, start_inode, path, 0, follow_symlinks)
    }

    /// Returns the parent Dir or Root that the file at a given path is in and the file name
    /// stripped off
    pub(crate) fn get_parent_inode_at_path(
        &self,
        inodes: &WasiInodes,
        base: WasiFd,
        path: &Path,
        follow_symlinks: bool,
    ) -> Result<(InodeGuard, String), Errno> {
        let mut parent_dir = std::path::PathBuf::new();
        let mut components = path.components().rev();
        let new_entity_name = components
            .next()
            .ok_or(Errno::Inval)?
            .as_os_str()
            .to_string_lossy()
            .to_string();
        for comp in components.rev() {
            parent_dir.push(comp);
        }
        self.get_inode_at_path(inodes, base, &parent_dir.to_string_lossy(), follow_symlinks)
            .map(|v| (v, new_entity_name))
    }

    pub fn get_fd(&self, fd: WasiFd) -> Result<Fd, Errno> {
        self.fd_map
            .read()
            .unwrap()
            .get(&fd)
            .ok_or(Errno::Badf)
            .map(|a| a.clone())
    }

    pub fn get_fd_inode(&self, fd: WasiFd) -> Result<InodeGuard, Errno> {
        self.fd_map
            .read()
            .unwrap()
            .get(&fd)
            .ok_or(Errno::Badf)
            .map(|a| a.inode.clone())
    }

    pub fn filestat_fd(&self, fd: WasiFd) -> Result<Filestat, Errno> {
        let inode = self.get_fd_inode(fd)?;
        let guard = inode.stat.read().unwrap();
        Ok(*guard.deref())
    }

    pub fn fdstat(&self, fd: WasiFd) -> Result<Fdstat, Errno> {
        match fd {
            __WASI_STDIN_FILENO => {
                return Ok(Fdstat {
                    fs_filetype: Filetype::CharacterDevice,
                    fs_flags: Fdflags::empty(),
                    fs_rights_base: STDIN_DEFAULT_RIGHTS,
                    fs_rights_inheriting: Rights::empty(),
                })
            }
            __WASI_STDOUT_FILENO => {
                return Ok(Fdstat {
                    fs_filetype: Filetype::CharacterDevice,
                    fs_flags: Fdflags::APPEND,
                    fs_rights_base: STDOUT_DEFAULT_RIGHTS,
                    fs_rights_inheriting: Rights::empty(),
                })
            }
            __WASI_STDERR_FILENO => {
                return Ok(Fdstat {
                    fs_filetype: Filetype::CharacterDevice,
                    fs_flags: Fdflags::APPEND,
                    fs_rights_base: STDERR_DEFAULT_RIGHTS,
                    fs_rights_inheriting: Rights::empty(),
                })
            }
            VIRTUAL_ROOT_FD => {
                return Ok(Fdstat {
                    fs_filetype: Filetype::Directory,
                    fs_flags: Fdflags::empty(),
                    // TODO: fix this
                    fs_rights_base: ALL_RIGHTS,
                    fs_rights_inheriting: ALL_RIGHTS,
                });
            }
            _ => (),
        }
        let fd = self.get_fd(fd)?;

        let guard = fd.inode.read();
        let deref = guard.deref();
        Ok(Fdstat {
            fs_filetype: match deref {
                Kind::File { .. } => Filetype::RegularFile,
                Kind::Dir { .. } => Filetype::Directory,
                Kind::Symlink { .. } => Filetype::SymbolicLink,
                _ => Filetype::Unknown,
            },
            fs_flags: fd.flags,
            fs_rights_base: fd.rights,
            fs_rights_inheriting: fd.rights_inheriting, // TODO(lachlan): Is this right?
        })
    }

    pub fn prestat_fd(&self, fd: WasiFd) -> Result<Prestat, Errno> {
        let inode = self.get_fd_inode(fd)?;
        //trace!("in prestat_fd {:?}", self.get_fd(fd)?);

        if inode.is_preopened {
            Ok(self.prestat_fd_inner(inode.deref()))
        } else {
            Err(Errno::Badf)
        }
    }

    pub(crate) fn prestat_fd_inner(&self, inode_val: &InodeVal) -> Prestat {
        Prestat {
            pr_type: Preopentype::Dir,
            u: PrestatEnum::Dir {
                // REVIEW:
                // no need for +1, because there is no 0 end-of-string marker
                // john: removing the +1 seems cause regression issues
                pr_name_len: inode_val.name.len() as u32 + 1,
            }
            .untagged(),
        }
    }

    #[allow(clippy::await_holding_lock)]
    pub async fn flush(&self, fd: WasiFd) -> Result<(), Errno> {
        match fd {
            __WASI_STDIN_FILENO => (),
            __WASI_STDOUT_FILENO => WasiInodes::stdout_mut(&self.fd_map)
                .map_err(fs_error_into_wasi_err)?
                .flush()
                .await
                .map_err(map_io_err)?,
            __WASI_STDERR_FILENO => WasiInodes::stderr_mut(&self.fd_map)
                .map_err(fs_error_into_wasi_err)?
                .flush()
                .await
                .map_err(map_io_err)?,
            _ => {
                let fd = self.get_fd(fd)?;
                if fd.rights.contains(Rights::FD_DATASYNC) {
                    return Err(Errno::Access);
                }

                let guard = fd.inode.read();
                match guard.deref() {
                    Kind::File {
                        handle: Some(file), ..
                    } => {
                        let mut file = file.write().unwrap();
                        file.flush().await.map_err(|_| Errno::Io)?
                    }
                    // TODO: verify this behavior
                    Kind::Dir { .. } => return Err(Errno::Isdir),
                    Kind::Buffer { .. } => (),
                    _ => return Err(Errno::Io),
                }
            }
        }
        Ok(())
    }

    /// Creates an inode and inserts it given a Kind and some extra data
    pub(crate) fn create_inode(
        &self,
        inodes: &WasiInodes,
        kind: Kind,
        is_preopened: bool,
        name: String,
    ) -> Result<InodeGuard, Errno> {
        let stat = self.get_stat_for_kind(&kind)?;
        Ok(self.create_inode_with_stat(inodes, kind, is_preopened, name.into(), stat))
    }

    /// Creates an inode and inserts it given a Kind, does not assume the file exists.
    pub(crate) fn create_inode_with_default_stat(
        &self,
        inodes: &WasiInodes,
        kind: Kind,
        is_preopened: bool,
        name: Cow<'static, str>,
    ) -> InodeGuard {
        let stat = Filestat::default();
        self.create_inode_with_stat(inodes, kind, is_preopened, name, stat)
    }

    /// Creates an inode with the given filestat and inserts it.
    pub(crate) fn create_inode_with_stat(
        &self,
        inodes: &WasiInodes,
        kind: Kind,
        is_preopened: bool,
        name: Cow<'static, str>,
        mut stat: Filestat,
    ) -> InodeGuard {
        match &kind {
            Kind::File {
                handle: Some(handle),
                ..
            } => {
                let guard = handle.read().unwrap();
                stat.st_size = guard.size();
            }
            Kind::Buffer { buffer } => {
                stat.st_size = buffer.len() as u64;
            }
            _ => {}
        }

        let ret = inodes.add_inode_val(InodeVal {
            stat: RwLock::new(stat),
            is_preopened,
            name,
            kind: RwLock::new(kind),
        });
        stat.st_ino = ret.ino().as_u64();
        ret
    }

    pub fn create_fd(
        &self,
        rights: Rights,
        rights_inheriting: Rights,
        flags: Fdflags,
        open_flags: u16,
        inode: InodeGuard,
    ) -> Result<WasiFd, Errno> {
        let idx = self.next_fd.fetch_add(1, Ordering::SeqCst);
        self.create_fd_ext(rights, rights_inheriting, flags, open_flags, inode, idx)?;
        Ok(idx)
    }

    pub fn create_fd_ext(
        &self,
        rights: Rights,
        rights_inheriting: Rights,
        flags: Fdflags,
        open_flags: u16,
        inode: InodeGuard,
        idx: WasiFd,
    ) -> Result<(), Errno> {
        let is_stdio = matches!(
            idx,
            __WASI_STDIN_FILENO | __WASI_STDOUT_FILENO | __WASI_STDERR_FILENO
        );
        self.fd_map.write().unwrap().insert(
            idx,
            Fd {
                rights,
                rights_inheriting,
                flags,
                offset: Arc::new(AtomicU64::new(0)),
                open_flags,
                inode,
                is_stdio,
            },
        );
        Ok(())
    }

    pub fn clone_fd(&self, fd: WasiFd) -> Result<WasiFd, Errno> {
        let fd = self.get_fd(fd)?;
        let idx = self.next_fd.fetch_add(1, Ordering::SeqCst);
        self.fd_map.write().unwrap().insert(
            idx,
            Fd {
                rights: fd.rights,
                rights_inheriting: fd.rights_inheriting,
                flags: fd.flags,
                offset: fd.offset.clone(),
                open_flags: fd.open_flags,
                inode: fd.inode,
                is_stdio: fd.is_stdio,
            },
        );
        Ok(idx)
    }

    /// Low level function to remove an inode, that is it deletes the WASI FS's
    /// knowledge of a file.
    ///
    /// This function returns the inode if it existed and was removed.
    ///
    /// # Safety
    /// - The caller must ensure that all references to the specified inode have
    ///   been removed from the filesystem.
    pub unsafe fn remove_inode(&self, inodes: &WasiInodes, ino: Inode) -> Option<Arc<InodeVal>> {
        let mut guard = inodes.protected.write().unwrap();
        guard.lookup.remove(&ino).and_then(|a| Weak::upgrade(&a))
    }

    fn create_stdout(&self, inodes: &WasiInodes) {
        self.create_std_dev_inner(
            inodes,
            Box::new(Stdout::default()),
            "stdout",
            __WASI_STDOUT_FILENO,
            STDOUT_DEFAULT_RIGHTS,
            Fdflags::APPEND,
        );
    }

    fn create_stdin(&self, inodes: &WasiInodes) {
        self.create_std_dev_inner(
            inodes,
            Box::new(Stdin::default()),
            "stdin",
            __WASI_STDIN_FILENO,
            STDIN_DEFAULT_RIGHTS,
            Fdflags::empty(),
        );
    }

    fn create_stderr(&self, inodes: &WasiInodes) {
        self.create_std_dev_inner(
            inodes,
            Box::new(Stderr::default()),
            "stderr",
            __WASI_STDERR_FILENO,
            STDERR_DEFAULT_RIGHTS,
            Fdflags::APPEND,
        );
    }

    fn create_std_dev_inner(
        &self,
        inodes: &WasiInodes,
        handle: Box<dyn VirtualFile + Send + Sync + 'static>,
        name: &'static str,
        raw_fd: WasiFd,
        rights: Rights,
        fd_flags: Fdflags,
    ) {
        let inode = {
            let stat = Filestat {
                st_filetype: Filetype::CharacterDevice,
                ..Filestat::default()
            };
            let kind = Kind::File {
                fd: Some(raw_fd),
                handle: Some(Arc::new(RwLock::new(handle))),
                path: "".into(),
            };
            inodes.add_inode_val(InodeVal {
                stat: RwLock::new(stat),
                is_preopened: true,
                name: name.to_string().into(),
                kind: RwLock::new(kind),
            })
        };
        self.fd_map.write().unwrap().insert(
            raw_fd,
            Fd {
                rights,
                rights_inheriting: Rights::empty(),
                flags: fd_flags,
                // since we're not calling open on this, we don't need open flags
                open_flags: 0,
                offset: Arc::new(AtomicU64::new(0)),
                inode,
                is_stdio: true,
            },
        );
    }

    pub fn get_stat_for_kind(&self, kind: &Kind) -> Result<Filestat, Errno> {
        let md = match kind {
            Kind::File { handle, path, .. } => match handle {
                Some(wf) => {
                    let wf = wf.read().unwrap();
                    return Ok(Filestat {
                        st_filetype: Filetype::RegularFile,
                        st_size: wf.size(),
                        st_atim: wf.last_accessed(),
                        st_mtim: wf.last_modified(),
                        st_ctim: wf.created_time(),

                        ..Filestat::default()
                    });
                }
                None => self
                    .root_fs
                    .metadata(path)
                    .map_err(fs_error_into_wasi_err)?,
            },
            Kind::Dir { path, .. } => self
                .root_fs
                .metadata(path)
                .map_err(fs_error_into_wasi_err)?,
            Kind::Symlink {
                base_po_dir,
                path_to_symlink,
                ..
            } => {
                let base_po_inode = &self.fd_map.read().unwrap()[base_po_dir].inode;
                let guard = base_po_inode.read();
                match guard.deref() {
                    Kind::Root { .. } => {
                        self.root_fs.symlink_metadata(path_to_symlink).map_err(fs_error_into_wasi_err)?
                    }
                    Kind::Dir { path, .. } => {
                        let mut real_path = path.clone();
                        // PHASE 1: ignore all possible symlinks in `relative_path`
                        // TODO: walk the segments of `relative_path` via the entries of the Dir
                        //       use helper function to avoid duplicating this logic (walking this will require
                        //       &self to be &mut sel
                        // TODO: adjust size of symlink, too
                        //      for all paths adjusted think about this
                        real_path.push(path_to_symlink);
                        self.root_fs.symlink_metadata(&real_path).map_err(fs_error_into_wasi_err)?
                    }
                    // if this triggers, there's a bug in the symlink code
                    _ => unreachable!("Symlink pointing to something that's not a directory as its base preopened directory"),
                }
            }
            _ => return Err(Errno::Io),
        };
        Ok(Filestat {
            st_filetype: virtual_file_type_to_wasi_file_type(md.file_type()),
            st_size: md.len(),
            st_atim: md.accessed(),
            st_mtim: md.modified(),
            st_ctim: md.created(),
            ..Filestat::default()
        })
    }

    /// Closes an open FD, handling all details such as FD being preopen
    pub(crate) fn close_fd(&self, fd: WasiFd) -> Result<(), Errno> {
        let mut fd_map = self.fd_map.write().unwrap();

        let pfd = fd_map.remove(&fd).ok_or(Errno::Badf);
        match pfd {
            Ok(fd_ref) => {
                let inode = fd_ref.inode.ino().as_u64();
                let ref_cnt = fd_ref.inode.ref_cnt();
                trace!(%fd, %inode, %ref_cnt, "closing file descriptor");
            }
            Err(err) => {
                trace!(%fd, "closing file descriptor failed - {}", err);
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for WasiFs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(guard) = self.current_dir.try_lock() {
            write!(f, "current_dir={} ", guard.as_str())?;
        } else {
            write!(f, "current_dir=(locked) ")?;
        }
        write!(f, "next_fd={} ", self.next_fd.load(Ordering::Relaxed))?;
        write!(f, "{:?}", self.root_fs)
    }
}

/// Returns the default filesystem backing
pub fn default_fs_backing() -> Box<dyn virtual_fs::FileSystem + Send + Sync> {
    cfg_if::cfg_if! {
        if #[cfg(feature = "host-fs")] {
            Box::new(virtual_fs::host_fs::FileSystem::default())
        } else if #[cfg(not(feature = "host-fs"))] {
            Box::new(virtual_fs::mem_fs::FileSystem::default())
        } else {
            Box::new(FallbackFileSystem::default())
        }
    }
}

#[derive(Debug, Default)]
pub struct FallbackFileSystem;

impl FallbackFileSystem {
    fn fail() -> ! {
        panic!("No filesystem set for wasmer-wasi, please enable either the `host-fs` or `mem-fs` feature or set your custom filesystem with `WasiEnvBuilder::set_fs`");
    }
}

impl FileSystem for FallbackFileSystem {
    fn read_dir(&self, _path: &Path) -> Result<virtual_fs::ReadDir, FsError> {
        Self::fail();
    }
    fn create_dir(&self, _path: &Path) -> Result<(), FsError> {
        Self::fail();
    }
    fn remove_dir(&self, _path: &Path) -> Result<(), FsError> {
        Self::fail();
    }
    fn rename(&self, _from: &Path, _to: &Path) -> Result<(), FsError> {
        Self::fail();
    }
    fn metadata(&self, _path: &Path) -> Result<virtual_fs::Metadata, FsError> {
        Self::fail();
    }
    fn symlink_metadata(&self, _path: &Path) -> Result<virtual_fs::Metadata, FsError> {
        Self::fail();
    }
    fn remove_file(&self, _path: &Path) -> Result<(), FsError> {
        Self::fail();
    }
    fn new_open_options(&self) -> virtual_fs::OpenOptions {
        Self::fail();
    }
}

pub fn virtual_file_type_to_wasi_file_type(file_type: virtual_fs::FileType) -> Filetype {
    // TODO: handle other file types
    if file_type.is_dir() {
        Filetype::Directory
    } else if file_type.is_file() {
        Filetype::RegularFile
    } else if file_type.is_symlink() {
        Filetype::SymbolicLink
    } else {
        Filetype::Unknown
    }
}

pub fn fs_error_from_wasi_err(err: Errno) -> FsError {
    match err {
        Errno::Badf => FsError::InvalidFd,
        Errno::Exist => FsError::AlreadyExists,
        Errno::Io => FsError::IOError,
        Errno::Addrinuse => FsError::AddressInUse,
        Errno::Addrnotavail => FsError::AddressNotAvailable,
        Errno::Pipe => FsError::BrokenPipe,
        Errno::Connaborted => FsError::ConnectionAborted,
        Errno::Connrefused => FsError::ConnectionRefused,
        Errno::Connreset => FsError::ConnectionReset,
        Errno::Intr => FsError::Interrupted,
        Errno::Inval => FsError::InvalidInput,
        Errno::Notconn => FsError::NotConnected,
        Errno::Nodev => FsError::NoDevice,
        Errno::Noent => FsError::EntryNotFound,
        Errno::Perm => FsError::PermissionDenied,
        Errno::Timedout => FsError::TimedOut,
        Errno::Proto => FsError::UnexpectedEof,
        Errno::Again => FsError::WouldBlock,
        Errno::Nospc => FsError::WriteZero,
        Errno::Notempty => FsError::DirectoryNotEmpty,
        _ => FsError::UnknownError,
    }
}

pub fn fs_error_into_wasi_err(fs_error: FsError) -> Errno {
    match fs_error {
        FsError::AlreadyExists => Errno::Exist,
        FsError::AddressInUse => Errno::Addrinuse,
        FsError::AddressNotAvailable => Errno::Addrnotavail,
        FsError::BaseNotDirectory => Errno::Notdir,
        FsError::BrokenPipe => Errno::Pipe,
        FsError::ConnectionAborted => Errno::Connaborted,
        FsError::ConnectionRefused => Errno::Connrefused,
        FsError::ConnectionReset => Errno::Connreset,
        FsError::Interrupted => Errno::Intr,
        FsError::InvalidData => Errno::Io,
        FsError::InvalidFd => Errno::Badf,
        FsError::InvalidInput => Errno::Inval,
        FsError::IOError => Errno::Io,
        FsError::NoDevice => Errno::Nodev,
        FsError::NotAFile => Errno::Inval,
        FsError::NotConnected => Errno::Notconn,
        FsError::EntryNotFound => Errno::Noent,
        FsError::PermissionDenied => Errno::Perm,
        FsError::TimedOut => Errno::Timedout,
        FsError::UnexpectedEof => Errno::Proto,
        FsError::WouldBlock => Errno::Again,
        FsError::WriteZero => Errno::Nospc,
        FsError::DirectoryNotEmpty => Errno::Notempty,
        FsError::Lock | FsError::UnknownError => Errno::Io,
    }
}
