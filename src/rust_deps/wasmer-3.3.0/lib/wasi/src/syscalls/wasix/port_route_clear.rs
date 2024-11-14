use super::*;
use crate::syscalls::*;

/// ### `port_route_clear()`
/// Clears all the routes in the local port
#[instrument(level = "debug", skip_all, ret, err)]
pub fn port_route_clear(mut ctx: FunctionEnvMut<'_, WasiEnv>) -> Result<Errno, WasiError> {
    let env = ctx.data();
    let net = env.net().clone();
    wasi_try_ok!(__asyncify(&mut ctx, None, async {
        net.route_clear().map_err(net_error_into_wasi_err)
    })?);
    Ok(Errno::Success)
}
