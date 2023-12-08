use rvemu::{cpu::Mode, emulator::Emulator, exception::Exception};
use std::error::Error;

mod cli;
mod input;
mod syscall;

/// Convert a RISC-V exception into an error.
pub fn exception_to_error(exc: Exception) -> Box<dyn Error> {
    format!("{:?}", exc).into()
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = cli::parse();

    let mut emu = Emulator::new();
    emu.cpu.mode = Mode::User;

    // Load the ELF binary into the emulator.
    let contents = std::fs::read(&cli.input)?;
    input::configure_emulator(&contents, &mut emu)?;

    let mut prev_pc = emu.cpu.pc;
    loop {
        emu.cpu.devices_increment();

        if let Some(interrupt) = emu.cpu.check_pending_interrupt() {
            interrupt.take_trap(&mut emu.cpu);

            // We don't do anything with the devices at the moment. So we'll
            // just panic if they magically come alive.
            panic!("Interrupt {:?}", interrupt);
        }

        emu.cpu
            .execute()
            .map(|_| ())
            .or_else(|exception| match exception {
                Exception::EnvironmentCallFromUMode => syscall::handle(&mut emu),

                _ => {
                    let trap = exception.take_trap(&mut emu.cpu);

                    // Don't bother handling other exceptions. For now they're
                    // all fatal.
                    panic!("Exception {:?} at {:#x}: {:?}", exception, prev_pc, trap)
                }
            })?;

        // If the program loops in place we assume it is stuck.
        if prev_pc == emu.cpu.pc {
            panic!("Stuck at {:#x}", prev_pc);
        }

        prev_pc = emu.cpu.pc;
    }
}
