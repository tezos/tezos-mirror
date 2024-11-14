use super::*;
use crate::syscalls::*;

/// ### `fd_advise()`
/// Advise the system about how a file will be used
/// Inputs:
/// - `Fd fd`
///     The file descriptor the advice applies to
/// - `Filesize offset`
///     The offset from which the advice applies
/// - `Filesize len`
///     The length from the offset to which the advice applies
/// - `__wasi_advice_t advice`
///     The advice to give
#[instrument(level = "debug", skip_all, fields(%fd, %offset, %len, ?advice), ret)]
pub fn fd_advise(
    ctx: FunctionEnvMut<'_, WasiEnv>,
    fd: WasiFd,
    offset: Filesize,
    len: Filesize,
    advice: Advice,
) -> Errno {
    // this is used for our own benefit, so just returning success is a valid
    // implementation for now
    Errno::Success
}
