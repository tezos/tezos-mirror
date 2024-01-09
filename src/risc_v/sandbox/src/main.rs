use kernel_loader::Memory;
use rvemu::{emulator::Emulator, exception::Exception};
use std::{error::Error, fs};

mod boot;
mod cli;
mod devicetree;
mod inbox;
mod input;
mod rv;
mod syscall;

/// Convert a RISC-V exception into an error.
pub fn exception_to_error(exc: Exception) -> Box<dyn Error> {
    format!("{:?}", exc).into()
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = cli::parse();

    let mut emu = Emulator::new();

    // Load the ELF binary into the emulator.
    let contents = std::fs::read(&cli.input)?;
    let initrd_addr = input::configure_emulator(&contents, &mut emu)?;

    // Load the initial ramdisk to memory.
    let initrd_info = cli
        .initrd
        .map(|initrd_path| -> Result<_, Box<dyn Error>> {
            let initrd = fs::read(initrd_path)?;
            emu.cpu.bus.write_bytes(initrd_addr, initrd.as_slice())?;
            Ok(devicetree::InitialRamDisk {
                start: initrd_addr,
                length: initrd.len() as u64,
            })
        })
        .transpose()?;

    // Generate and load the flattened device tree.
    let dtb_addr = initrd_info
        .as_ref()
        .map(|info| info.start + info.length)
        .unwrap_or(initrd_addr);
    let dtb = devicetree::generate(initrd_info)?;
    emu.cpu.bus.write_bytes(dtb_addr, dtb.as_slice())?;

    // Prepare the boot procedure
    boot::configure(&mut emu, dtb_addr);

    // Prepare inbox
    let mut inbox = inbox::InboxBuilder::new();
    inbox
        .insert_external(vec![1, 2, 3, 4])
        .insert_external(vec![1, 4, 3, 2])
        .next_level()
        .insert_external(vec![1, 1])
        .next_level()
        .insert_external(vec![1, 2]);
    let mut inbox = inbox.build();

    let handle_syscall = if cli.posix {
        fn dummy(emu: &mut Emulator, _: &mut inbox::Inbox) -> Result<(), Box<dyn Error>> {
            syscall::handle_posix(emu)
        }
        dummy
    } else {
        syscall::handle_sbi
    };

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
                    Exception::EnvironmentCallFromSMode | Exception::EnvironmentCallFromUMode => {
                        handle_syscall(&mut emu, &mut inbox).map_err(|err| -> Box<dyn Error> {
                            format!("Failed to handle environment call at {prev_pc:x}: {}", err)
                                .as_str()
                                .into()
                        })?;

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
