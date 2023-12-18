use kernel_loader::Memory;
use rvemu::{cpu::Mode, emulator::Emulator, exception::Exception};
use std::error::Error;

mod cli;
mod devicetree;
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
    let dtb_addr = input::configure_emulator(&contents, &mut emu)?;

    // Generate and load the flattened device tree.
    let dtb = devicetree::generate(None)?;
    emu.cpu.bus.write_bytes(dtb_addr, dtb.as_slice())?;

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
            .or_else(|exception| -> Result<(), Box<dyn Error>> {
                match exception {
                    Exception::EnvironmentCallFromUMode => {
                        syscall::handle(&mut emu)?;

                        // We need to update the program counter ourselves now.
                        // This is a recent change in behaviour in RVEmu.
                        emu.cpu.pc += 4;

                        Ok(())
                    }

                    _ => {
                        let trap = exception.take_trap(&mut emu.cpu);

                        // Don't bother handling other exceptions. For now they're
                        // all fatal.
                        panic!("Exception {:?} at {:#x}: {:?}", exception, prev_pc, trap)
                    }
                }
            })?;

        // If the program loops in place we assume it is stuck.
        if prev_pc == emu.cpu.pc {
            panic!("Stuck at {:#x}", prev_pc);
        }

        prev_pc = emu.cpu.pc;
    }
}
