use super::*;
use crate::syscalls::*;

/// ### `fd_dup()`
/// Duplicates the file handle
/// Inputs:
/// - `Fd fd`
///   File handle to be cloned
/// Outputs:
/// - `Fd fd`
///   The new file handle that is a duplicate of the original
#[instrument(level = "debug", skip_all, fields(%fd, ret_fd = field::Empty), ret)]
pub fn fd_dup<M: MemorySize>(
    ctx: FunctionEnvMut<'_, WasiEnv>,
    fd: WasiFd,
    ret_fd: WasmPtr<WasiFd, M>,
) -> Errno {
    let env = ctx.data();
    let (memory, state) = env.get_memory_and_wasi_state(&ctx, 0);
    let fd = wasi_try!(state.fs.clone_fd(fd));

    Span::current().record("ret_fd", fd);
    wasi_try_mem!(ret_fd.write(&memory, fd));

    Errno::Success
}
