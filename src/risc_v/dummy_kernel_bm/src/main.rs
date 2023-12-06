#![cfg_attr(target_os = "none", no_std)]
#![cfg_attr(target_os = "none", no_main)]

extern crate alloc;

#[cfg(not(target_os = "none"))]
extern crate std;

mod actual_main;
mod dumb_alloc;

use tezos_smart_rollup::core_unsafe::rollup_host::RollupHost;

#[inline]
fn main_wrapper() -> ! {
    #[cfg(target_arch = "riscv64")]
    use tezos_smart_rollup::core_unsafe::riscv64_syscalls::exit;

    #[cfg(not(target_arch = "riscv64"))]
    use std::process::exit;

    let host = unsafe { RollupHost::new() };
    crate::actual_main::main(host);
    exit(0)
}

#[cfg(all(target_arch = "riscv64", target_os = "none"))]
mod bare_metal {
    use crate::dumb_alloc;
    use tezos_smart_rollup::core_unsafe::riscv64_syscalls::exit;

    // This code runs before the main.
    #[riscv_rt::pre_init]
    unsafe fn pre_init() {
        dumb_alloc::init();
    }

    // We need a custom panic handler to ensure fatal errors are visible to the
    // outside world.
    #[panic_handler]
    pub fn panic_handler(info: &core::panic::PanicInfo) -> ! {
        tezos_smart_rollup_panic_hook::panic_handler(info);
        exit(1)
    }

    // When targeting RISC-V bare-metal we need a custom entrypoint mechanism.
    // Fortunateky, riscv-rt provides this for us.
    #[allow(non_snake_case)]
    #[riscv_rt::entry]
    unsafe fn main() -> ! {
        super::main_wrapper()
    }
}

// We can re-use the default mechanism around entrypoints when we're not
// compiling to the bare-metal target.
#[cfg(not(target_os = "none"))]
fn main() {
    main_wrapper();
}
