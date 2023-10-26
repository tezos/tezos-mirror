#![cfg_attr(target_os = "none", no_std)]
#![cfg_attr(target_os = "none", no_main)]

extern crate alloc;

#[cfg(not(target_os = "none"))]
extern crate std;

mod actual_main;
mod dumb_alloc;
mod syscalls;

#[inline]
fn main_wrapper() -> ! {
    crate::actual_main::main();
    crate::syscalls::exit(0)
}

#[cfg(target_os = "none")]
mod bare_metal {
    use crate::{
        dumb_alloc,
        syscalls::{exit, write_str, StdErr},
    };

    // This code runs before the main.
    #[riscv_rt::pre_init]
    unsafe fn pre_init() {
        dumb_alloc::init();
    }

    // We need a custom panic handler to ensure fatal errors are visible to the
    // outside world.
    #[panic_handler]
    pub fn panic_handler(info: &core::panic::PanicInfo) -> ! {
        write_str(StdErr, "Panic at ");

        if let Some(loc) = info.location() {
            let location = alloc::format!("{}: ", loc);
            write_str(StdErr, location);
        } else {
            write_str(StdErr, "<unknown>: ");
        }

        // Unfortunately the following section only works when TypeIds work.
        // TypeIds appear to not work for some reason when targeting bare-metal
        // RISC-V.
        #[cfg(feature = "typeids")]
        {
            let message =
                if let Some(message) = info.payload().downcast_ref::<alloc::string::String>() {
                    message.as_str()
                } else {
                    let message = info.payload().downcast_ref::<&str>();
                    message.unwrap_or(&"<unknown message>")
                };

            write_str(StdErr, message);
        }

        #[cfg(not(feature = "typeids"))]
        {
            write_str(StdErr, "<unknown>");
        }

        write_str(StdErr, "\n");

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
